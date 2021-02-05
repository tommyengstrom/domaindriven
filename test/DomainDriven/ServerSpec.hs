{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.ServerSpec where

import           DomainDriven.Server
import           Prelude
import           GHC.Generics                   ( Generic )
import           Test.Hspec
import           Data.Aeson
import           Data.Char                      ( toLower )
import           Control.Concurrent.Async
import           DomainDriven.Persistance.ForgetfulSTM
import           DomainDriven
import           Control.Exception              ( SomeException )
import           Servant
import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           Data.OpenApi                   ( ToSchema )
import           Servant.Client
import           DomainDriven.ServerOptions

newtype ItemKey = ItemKey String
    deriving newtype (Show, Eq)
    deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, HasFieldName)

newtype Price = Euros Int
    deriving newtype (Show, Eq, FromJSON, ToJSON)

data StoreCmd a where
    AddToCart  ::ItemKey -> StoreCmd ()
    ItemAction ::Int -> ItemCmd a -> StoreCmd a

data ItemCmd a where
    SubOp1 ::ItemCmd ()
    SubOp2 ::Int -> String -> ItemCmd Int

data Item = Item
    { key         :: ItemKey
    , description :: String
    , price       :: Price
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data StoreLookup a where
    LookupAll ::StoreLookup [Item]
    LookupItem ::ItemKey -> StoreLookup Item
    MultipleParameters ::ItemKey -> String -> Int -> StoreLookup Item


$(mkCmdServer testServerOptions ''StoreCmd)
$(mkQueryServer defaultServerOptions ''StoreLookup)

type StoreModel = ()
type StoreEvent = ()


handleStoreCmd :: StoreCmd a -> IO (StoreModel -> Either SomeException (a, [StoreEvent]))
handleStoreCmd = \case
    AddToCart _       -> pure (const $ Right ((), []))
    ItemAction _ icmd -> handleItemCmd icmd

handleItemCmd :: ItemCmd a -> IO (StoreModel -> Either SomeException (a, [StoreEvent]))
handleItemCmd = \case
    SubOp1     -> pure . const $ Right ((), [])
    SubOp2 i _ -> pure . const $ Right (i, [])

applyStoreEvent :: StoreModel -> Stored StoreEvent -> StoreModel
applyStoreEvent _ _ = ()


-- | Compare with StoreCmdApi if you run into issues
type ExpectedStoreCmdApi
    = "cart" :> "add" :> ReqBody '[JSON] (NamedFields1 "Add" ItemKey) :> Post '[JSON] NoContent
    :<|> Capture "Int" Int :> "subop1" :> Post '[JSON] NoContent

cartAdd :: NamedFields1 "Add" ItemKey -> ClientM NoContent
itemSubOp1 :: Int -> ClientM NoContent
cartAdd :<|> itemSubOp1 = client (Proxy @ExpectedStoreCmdApi)

spec :: Spec
spec = do
    runIO $ do
        p <- createForgetfulSTM applyStoreEvent ()
        _ <- async . run 9898 $ serve (Proxy @StoreCmdApi)
                                      (storeCmdServer $ runCmd p handleStoreCmd)
        pure ()
    clientEnv <- runIO $ do
        man <- newManager defaultManagerSettings
        pure $ mkClientEnv man (BaseUrl Http "localhost" 9898 "")
    describe "Server endpoint renaming" $ do
        it "Can split constructor into multiple path segments" $ do
            r <- runClientM (cartAdd $ NamedFields1 $ ItemKey "whatever") clientEnv
            r `shouldBe` Right NoContent
        it "Can remove constructor name" $ do
            r <- runClientM (itemSubOp1 1) clientEnv
            r `shouldBe` Right NoContent
