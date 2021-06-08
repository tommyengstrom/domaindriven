{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.ServerSpec where

import           DomainDriven.Server
import           Prelude
import           Test.Hspec
import           Control.Monad.Except
import           Test.Hspec.Core.Hooks
import           Control.Concurrent.Async
import           Control.Concurrent
import           DomainDriven.Persistance.ForgetfulInMemory
import           DomainDriven
import           Servant
import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           Servant.Client
import qualified Data.Text                                    as T
import           StoreModel
import           Data.Text                      ( Text )
import           Control.Monad.Reader

data Stuff = Stuff
    { theSecretSause :: Text
    }
    deriving Show


$(mkServer (Proxy @()) domaindrivenServerConfig ''StoreAction)

buyItem :: NamedFields2 "StoreAction_BuyItem" ItemKey Quantity -> ClientM NoContent
listItems :: ClientM [ItemInfo]
search :: Text -> ClientM [ItemInfo]
stockQuantity :: ItemKey -> ClientM Quantity
restock :: NamedFields2 "AdminAction_Restock" ItemKey Quantity -> ClientM NoContent
addItem :: NamedFields3 "AdminAction_AddItem" ItemName Quantity Price -> ClientM ItemKey
removeItem :: NamedFields1 "AdminAction_RemoveItem" ItemKey -> ClientM NoContent

buyItem :<|> listItems :<|> search :<|> stockQuantity :<|> (restock :<|> addItem :<|> removeItem)
    = client $ Proxy @StoreActionApi

data TestAction method a where
    ReverseText ::Text -> TestAction (RequestType '[PlainText] (Verb 'POST 200 '[JSON])) Text
    SecretSauce ::TestAction Query Text

type ExpectedReverseText
    = "ReverseText" :> ReqBody '[PlainText] Text :> Post '[JSON] Text

expectedReverseText :: Text -> ClientM Text
expectedReverseText = client (Proxy @ExpectedReverseText)

handleTestAction :: ActionHandler () () TestAction (ReaderT Stuff IO)
handleTestAction = \case
    ReverseText t -> Cmd $ \() -> pure (T.reverse t, [])
    SecretSauce   -> Query $ \_ -> asks theSecretSause


$(mkServer (Proxy @()) defaultServerConfig ''TestAction)

withStoreServer :: IO () -> IO ()
withStoreServer runTests = do
    p      <- createForgetful applyStoreEvent mempty
    -- server <- async . run 9898 $ serve (Proxy @StoreActionApi) undefined
    server <- async . run 9898 $ serve (Proxy @StoreActionApi) $ hoistServer
        (Proxy @StoreActionApi)
        (\m -> Handler $ ExceptT $ fmap Right $ runReaderT m ())
        (storeActionServer $ runAction p handleStoreAction)
    threadDelay 10000 -- Ensure the server is live when the tests run
    runTests
    cancel server

withTestServer :: IO () -> IO ()
withTestServer runTests = do
    p      <- createForgetful (\m _ -> m) ()
    -- server <- async . run 9898 $ serve (Proxy @StoreActionApi) undefined
    server <- async . run 9898 $ serve (Proxy @TestActionApi) $ hoistServer
        (Proxy @TestActionApi)
        (\m -> Handler $ ExceptT $ fmap Right $ runReaderT m (Stuff "ketchup"))
        (testActionServer $ runAction p handleTestAction)
    threadDelay 10000 -- Ensure the server is live when the tests run
    runTests
    cancel server

spec :: Spec
spec = do
    clientEnv <- runIO $ do
        man <- newManager defaultManagerSettings
        pure $ mkClientEnv man (BaseUrl Http "localhost" 9898 "")
    aroundAll_ withStoreServer $ do

        describe "Server endpoint renaming" $ do
            it "Can add item" $ do
                r <- runClientM (addItem $ NamedFields3 "Test item" 10 99) clientEnv
                r `shouldSatisfy` not . null
            it "The new item shows up when listing items" $ do
                r <- runClientM listItems clientEnv
                case r of
                    Right [ItemInfo _ n _ _] -> n `shouldBe` "Test item"
                    a                        -> fail $ "That shouldn't happen! " <> show a
    aroundAll_ withTestServer $ do
        describe "Alternating content types" $ do
            it "Plaintext endpoint works" $ do
                r <- runClientM (expectedReverseText "Hej") clientEnv
                r `shouldBe` Right "jeH"
    describe "Testing the reader" $ do
        it "Gives the secret source" $ do
            p <- createForgetful const ()
            r <- runReaderT (runAction p handleTestAction SecretSauce)
                            (Stuff "this is secret")
            r `shouldBe` "this is secret"
