{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
module StoreModel where


import qualified Data.Map                                     as M
import           DomainDriven
import           DomainDriven.Server
import           DomainDriven.Persistance.ForgetfulInMemory
import           Data.Typeable
import           Prelude
import qualified Data.Text                                    as T
import           Data.Aeson                     ( encode
                                                , ToJSON
                                                , FromJSON
                                                , FromJSONKey
                                                , ToJSONKey
                                                )
import           Data.Text                      ( Text )
import           Control.Monad.Catch            ( throwM )
import           GHC.Generics                   ( Generic )
import           Control.Monad                  ( when )
import qualified Data.ByteString.Lazy.Char8                   as BL
import           Data.String                    ( IsString )
import           Data.OpenApi                   ( ToSchema
                                                , ToParamSchema
                                                )
import           Servant.OpenApi
import           Network.Wai.Handler.Warp       ( run
                                                , Port
                                                )
import           Servant

------------------------------------------------------------------------------------------
-- Defining the types we need                                                           --
-- `HasFieldName` defines the name the type will get in the request body.               --
------------------------------------------------------------------------------------------
newtype ItemKey = ItemKey UUID
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSONKey, ToJSONKey, FromJSON, ToJSON, ToSchema, HasFieldName, ToParamSchema)
    deriving newtype (FromHttpApiData, ToHttpApiData)
newtype Quantity = Quantity Int
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Num)
    deriving anyclass (FromJSON, ToJSON, ToSchema, HasFieldName)
newtype ItemName = ItemName Text
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, HasFieldName)
    deriving newtype (IsString)
newtype Price = Price Int
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Num)
    deriving anyclass (FromJSON, ToJSON, ToSchema, HasFieldName)

data ItemInfo = ItemInfo
    { key      :: ItemKey
    , name     :: ItemName
    , quantity :: Quantity
    , price    :: Price
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
-- | The store actions
-- `method` is `Verb` from servant without the returntype, `a`, applied
data StoreAction method a where
    BuyItem    ::ItemKey -> Quantity -> StoreAction CMD ()
    ListItems ::StoreAction (Verb 'GET 200 '[JSON]) [ItemInfo]
    Search ::Text -> StoreAction QUERY [ItemInfo]
    ItemAction ::ItemKey -> ItemAction method a -> StoreAction method a
    AdminAction ::AdminAction method a -> StoreAction method a -- ^ Sub-actions

data ItemAction method a where
    StockQuantity ::ItemAction QUERY Quantity

data AdminAction method a where
    Restock    ::ItemKey -> Quantity -> AdminAction CMD ()
    AddItem    ::ItemName -> Quantity -> Price -> AdminAction CMD ItemKey
    RemoveItem ::ItemKey -> AdminAction CMD ()

-- | The event
-- Store state of the store is fully defined by
-- `foldl' applyStoreEvent mempty listOfEvents`
data StoreEvent
    = BoughtItem ItemKey Quantity
    | Restocked ItemKey Quantity
    | AddedItem ItemKey ItemName Price
    | RemovedItem ItemKey
    deriving stock (Show, Eq, Generic, Typeable)
    deriving (FromJSON, ToJSON) via (NamedJsonFields StoreEvent)

type StoreModel = M.Map ItemKey ItemInfo

------------------------------------------------------------------------------------------
-- Action handlers                                                                      --
------------------------------------------------------------------------------------------
handleStoreAction :: CmdHandler StoreModel StoreEvent StoreAction
handleStoreAction = \case
    BuyItem iKey quantity' -> Cmd $ \m -> do
        let available = maybe 0 quantity $ M.lookup iKey m
        when (available < quantity') $ throwM err422 { errBody = "Out of stock" }
        pure ((), [BoughtItem iKey quantity'])
    ListItems -> Query $ pure . M.elems
    Search t  -> Query $ \m -> do
        let matches :: ItemInfo -> Bool
            matches (ItemInfo _ (ItemName n) _ _) = T.toUpper t `T.isInfixOf` T.toUpper n
        pure . filter matches $ M.elems m
    AdminAction cmd     -> handleAdminAction cmd
    ItemAction iKey cmd -> handleItemAction iKey cmd

handleAdminAction :: CmdHandler StoreModel StoreEvent AdminAction
handleAdminAction = \case
    Restock iKey q -> Cmd $ \m -> do
        when (M.notMember iKey m) $ throwM err404
        pure ((), [Restocked iKey q])
    AddItem name' quantity' price -> Cmd $ \_ -> do
        iKey <- ItemKey <$> mkId
        pure (iKey, [AddedItem iKey name' price, Restocked iKey quantity'])
    RemoveItem iKey -> Cmd $ \m -> do
        when (M.notMember iKey m) $ throwM err404
        pure ((), [RemovedItem iKey])

handleItemAction :: ItemKey -> CmdHandler StoreModel StoreEvent ItemAction
handleItemAction iKey = \case
    StockQuantity -> Query $ \m -> do
        i <- getItem m
        pure $ quantity i
  where
    getItem :: StoreModel -> IO ItemInfo
    getItem = maybe (throwM err404) pure . M.lookup iKey
------------------------------------------------------------------------------------------
-- Event handler                                                                        --
------------------------------------------------------------------------------------------
applyStoreEvent :: StoreModel -> Stored StoreEvent -> StoreModel
applyStoreEvent m (Stored e _ _) = case e of
    BoughtItem iKey q -> M.update (\ii -> Just ii { quantity = quantity ii - q }) iKey m
    Restocked iKey q -> M.update (\ii -> Just ii { quantity = quantity ii + q }) iKey m
    AddedItem iKey name' price -> M.insert iKey (ItemInfo iKey name' 0 price) m
    RemovedItem iKey -> M.delete iKey m



------------------------------------------------------------------------------------------
-- Defining the server                                                                  --
------------------------------------------------------------------------------------------
$(mkServer defaultApiOptions ''StoreAction)

--
app :: (WriteModel p, Model p ~ StoreModel, Event p ~ StoreEvent) => Port -> p -> IO ()
app port wm = do
    putStrLn $ "Starting server on port: " <> show port
    BL.writeFile "/tmp/store_schema.json" . encode . toOpenApi $ Proxy @StoreActionApi
    run port $ serve (Proxy @StoreActionApi)
                     (storeActionServer $ runCmd wm handleStoreAction)

forgetfulApp :: Port -> IO ()
forgetfulApp p = app p =<< createForgetful applyStoreEvent mempty
