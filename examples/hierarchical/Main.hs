{-# LANGUAGE TemplateHaskell #-}

-- | This module contains simple example of how to write hierarchical models in
-- domain-driven. Note that in real life you may not want to split these models up. The
-- intent of this example is just to show the technique.
module Main where

import           DomainDriven.Server            ( mkCmdServer
                                                , mkQueryServer
                                                )
import           DomainDriven
import           Prelude
import           Data.Bifunctor                 ( bimap )
import           Servant                        ( serve
                                                , Capture
                                                , FromHttpApiData
                                                , Proxy(..)
                                                , Server
                                                , (:<|>)(..)
                                                )
import           Data.Typeable                  ( Typeable )
import           Control.Exception              ( Exception )
import           Network.Wai.Handler.Warp       ( run )
import           DomainDriven.Persistance.FileAndSTM
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Map                                     as M
import           Control.Monad
import           Control.Exception              ( throwIO )
import           Servant.Docs
import           Data.UUID                      ( nil )
------------------------------------------------------------------------------------------
-- Item model ----------------------------------------------------------------------------
------------------------------------------------------------------------------------------
data Item = Item
    { description :: Description
    , price       :: Price
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype ItemKey = ItemKey UUID
    deriving newtype (Show, Eq, Ord, FromJSON, ToJSON, FromHttpApiData)

newtype Description = Description String
    deriving newtype (Show, Eq, FromJSON, ToJSON)

newtype Price = EUR Int
    deriving newtype (Show, Eq, Ord, Num, FromJSON, ToJSON)

data ItemCmd a where
    ChangeDescription ::Description -> ItemCmd ()
    ChangePrice ::Price -> ItemCmd ()

data ItemEvent
    = ChangedDescription Description
    | ChangedPrice Price
    deriving (Show, Generic, FromJSON, ToJSON)

applyItemEvent :: Item -> Stored ItemEvent -> Item
applyItemEvent m (Stored e _ _) = case e of
    ChangedDescription s -> m { description = s }
    ChangedPrice       p -> m { price = p }

data ItemError = PriceMustBePositive
    deriving (Show, Eq, Typeable, Exception)

handleItemCmd :: CmdHandler Item ItemEvent ItemCmd ItemError
handleItemCmd = \case
    ChangeDescription s -> pure $ \_ -> Right ((), [ChangedDescription s])
    ChangePrice       p -> do
        when (p < 0) $ throwIO PriceMustBePositive
        -- Throwing the exception while in IO yields the same result as returning
        -- `Left ItemError` in the continuation.
        pure $ \_ -> Right ((), [ChangedPrice p])



------------------------------------------------------------------------------------------
-- Store model ---------------------------------------------------------------------------
------------------------------------------------------------------------------------------
type StoreModel = M.Map ItemKey Item

data StoreCmd a where
   AddItem ::Item -> StoreCmd ()
   RemoveItem ::ItemKey -> StoreCmd ()
   UpdateItem ::ItemKey -> ItemCmd a -> StoreCmd a

data StoreEvent
    = AddedItem ItemKey Item
    | RemovedItem ItemKey
    | UpdatedItem ItemKey ItemEvent
    deriving (Show, Generic, FromJSON, ToJSON)

data StoreError
    = NoSuchItem
    | StoreItemError ItemError
    deriving (Show, Eq, Typeable, Exception)

applyStoreEvent :: StoreModel -> Stored StoreEvent -> StoreModel
applyStoreEvent m se@(Stored event _timestamp _uuid) = case event of
    AddedItem iKey i -> M.insert iKey i m
    RemovedItem iKey -> M.delete iKey m
    UpdatedItem iKey iEvent ->
        M.adjust (`applyItemEvent` se { storedEvent = iEvent }) iKey m

handleStoreCmd :: CmdHandler StoreModel StoreEvent StoreCmd StoreError
handleStoreCmd = \case
    AddItem i -> do
        iKey <- ItemKey <$> mkId
        pure $ \_ -> Right ((), [AddedItem iKey i])
    RemoveItem iKey -> pure $ \m -> case M.lookup iKey m of
        Just _  -> Right ((), [RemovedItem iKey])
        Nothing -> Left NoSuchItem
    UpdateItem iKey iCmd -> do
        -- First we have to run the
        itemContinuation <- handleItemCmd iCmd
        pure $ \m -> case M.lookup iKey m of
            Just i ->
                -- We now need to extract the Item data and send it to `itemContinuation`.
                -- After this is done we need to convert `ItemError` to `StoreError` and
                -- `ItemEvent` to `StoreEvent`
                bimap
                        StoreItemError
                        (\(returnValue, listOfEvents) ->
                            (returnValue, fmap (UpdatedItem iKey) listOfEvents)
                        )
                    $ itemContinuation i
            Nothing -> Left NoSuchItem

$(mkCmdServer ''StoreCmd)

------------------------------------------------------------------------------------------
-- Store queries -------------------------------------------------------------------------
------------------------------------------------------------------------------------------

data StoreQuery a where
    ListAllItems ::StoreQuery [(ItemKey, Item)]
    LookupItem ::ItemKey -> StoreQuery Item

runStoreQuery :: StoreModel -> StoreQuery a -> IO (Either StoreError a)
runStoreQuery m = \case
    ListAllItems    -> pure . Right $ M.toList m
    LookupItem iKey -> pure $ maybe (Left NoSuchItem) Right $ M.lookup iKey m

$(mkQueryServer ''StoreQuery)

-- We can assemble the individual APIs as we would with any other Servant APIs.
type Api = StoreCmdApi :<|> StoreQueryApi

-- The complete server require both the a CommandRunner and a QueryRunner
server :: QueryRunner StoreQuery -> CmdRunner StoreCmd -> Server Api
server queryRunner cmdRunner = storeCmdServer cmdRunner :<|> storeQueryServer queryRunner


------------------------------------------------------------------------------------------
-- Servant-docs for documentation --------------------------------------------------------
------------------------------------------------------------------------------------------
instance ToCapture (Capture "ItemKey" ItemKey) where
    toCapture _ = DocCapture "ItemKey" "Item Id"

instance ToCapture (Capture "Price" Price) where
    toCapture _ = DocCapture "Price" "Price in Euroes"

instance ToSample ItemKey where
    toSamples _ = [("key", ItemKey nil)]

instance ToSample Price where
    toSamples _ = [("a cheap thing", EUR 3)]

instance ToSample Item where
    toSamples _ = [("item", Item (Description "sample item") (EUR 35))]


instance ToSample Description where
    toSamples _ = []
-- | Start a server running on port 8765
main :: IO ()
main = do
    -- Then we need to create the model
    dm <- createFileAndSTM "/tmp/hierarcicalevents.sjson" applyStoreEvent mempty

    -- Print the API documentation before starting the server
    putStrLn . markdown . docs $ Proxy @Api
    -- Now we can supply the CmdRunner to the generated server and run it as any other
    -- Servant server.
    run 8765 $ serve (Proxy @Api)
                     (server (runQuery dm runStoreQuery) (runCmd dm handleStoreCmd))
