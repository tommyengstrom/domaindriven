{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module contains simple example of how to write hierarchical models in
-- domain-driven. Note that in real life you may not want to split these models up. The
-- intent of this example is just to show the technique.
module Main where

import           DomainDriven.Server
import           DomainDriven
import           Prelude
import           Data.Bifunctor                 ( bimap )
import           Servant                 hiding ( Description )
import           Data.Typeable                  ( Typeable )
import           Control.Exception              ( Exception )
import           Network.Wai.Handler.Warp       ( run )
import           DomainDriven.Persistance.FileWithSTM
import           GHC.Generics                   ( Generic )
import qualified Data.ByteString.Lazy.Char8                   as BL
import           Data.Aeson                     ( FromJSON
                                                , encode
                                                , ToJSON
                                                )
import qualified Data.Map                                     as M
import           Control.Monad
import           Control.Exception              ( throwIO )
import           Data.OpenApi                   ( ToSchema
                                                , ToParamSchema
                                                )
import           Servant.OpenApi




data StoreCmd a where
--  AddItem ::Item -> StoreCmd ()
--  RemoveItem ::ItemKey -> StoreCmd ()
--  UpdateItem ::ItemCmd a -> StoreCmd a
  ManipulateItem ::ItemKey -> ItemCmd a -> StoreCmd a


data ItemCmd a where
  ChangeDescription ::Description -> ItemCmd ()
  ChangePrice ::Price -> ItemCmd ()

------------------------------------------------------------------------------------------
-- Item model ----------------------------------------------------------------------------
------------------------------------------------------------------------------------------
data Item = Item
    { description :: Description
    , price       :: Price
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema, HasFieldName)

newtype ItemKey = ItemKey UUID
    deriving newtype (Show, Eq, Ord, FromJSON, ToJSON, FromHttpApiData, ToSchema, ToParamSchema)
    deriving stock (Generic)
    deriving anyclass (HasFieldName)

newtype Description = Description String
    deriving newtype (Show, Eq, FromJSON, ToJSON, ToSchema)
    deriving stock (Generic)
    deriving anyclass (HasFieldName)

newtype Price = EUR Int
    deriving newtype (Show, Eq, Ord, Num, FromJSON, ToJSON, ToSchema)
    deriving stock (Generic)
    deriving anyclass (HasFieldName)

data ItemEvent
    = ChangedDescription Description
    | ChangedPrice Price
    deriving (Show, Generic, FromJSON, ToJSON)

newtype SearchTerm = SearchTerm String
    deriving newtype (Show, Eq, Ord, FromJSON, ToJSON, ToSchema, FromHttpApiData)
    deriving stock (Generic)
    deriving anyclass (HasFieldName)

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
    --AddItem i -> do
    --    putStrLn "AddItem"
    --    iKey <- ItemKey <$> mkId
    --    pure $ \_ -> Right ((), [AddedItem iKey i])
    --RemoveItem iKey -> do
    --    putStrLn "RemoveItem"
    --    pure $ \m -> case M.lookup iKey m of
    --        Just _  -> Right ((), [RemovedItem iKey])
    --        Nothing -> Left NoSuchItem
    --UpdateItem _       -> putStrLn "UpdateItem" *> undefined
    ManipulateItem _ _ -> putStrLn "ManipulateItem" *> undefined

$(mkCmdServer defaultApiOptions ''StoreCmd)








------------------------------------------------------------------------------------------
-- Store queries -------------------------------------------------------------------------
------------------------------------------------------------------------------------------

--data StoreQuery a where
--    ListItems ::Maybe SearchTerm -> StoreQuery [(ItemKey, Item)]
--    LookupItem ::ItemKey -> StoreQuery Item
--
--runStoreQuery :: QueryHandler StoreModel StoreQuery StoreError
--runStoreQuery m = \case
--    ListItems  _    -> pure . Right $ M.toList m
--    LookupItem iKey -> pure $ maybe (Left NoSuchItem) Right $ M.lookup iKey m
--
-- $(mkQueryServer defaultApiOptions ''StoreQuery)

-- We can assemble the individual APIs as we would with any other Servant APIs.
type Api = StoreCmdApi -- :<|> StoreQueryApi

-- The complete server require both the a CommandRunner and a QueryRunner
--server :: QueryRunner StoreQuery -> CmdRunner StoreCmd -> Server Api
--server queryRunner cmdRunner = storeCmdServer cmdRunner :<|> storeQueryServer queryRunner
server :: CmdRunner StoreCmd -> Server Api
server = storeCmdServer


-- | Start a server running on port 8765
main :: IO ()
main = do
    -- Then we need to create the model
    dm <- createFileWithSTM "/tmp/hierarcicalevents.sjson" applyStoreEvent mempty

    BL.writeFile "/tmp/0000test_api.json" . encode . toOpenApi $ Proxy @StoreCmdApi
    -- Print the API documentation before starting the server
    -- Now we can supply the CmdRunner to the generated server and run it as any other
    -- Servant server.
    run 8888 $ serve (Proxy @Api) (server (runCmd dm handleStoreCmd))
