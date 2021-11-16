{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.ServerSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Data.Text                      ( Text )
import           DomainDriven
import           DomainDriven.Persistance.ForgetfulInMemory
import           DomainDriven.Server
import           DomainDriven.ServerSpecModel
import           Network.HTTP.Client            ( defaultManagerSettings
                                                , newManager
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           Prelude
import           Servant
import           Servant.Client
import           StoreModel
import           Test.Hspec
import           Test.Hspec.Core.Hooks

$(mkServer storeActionConfig ''StoreAction)

buyItem :: StoreAction_BuyItemBody -> ClientM NoContent
listItems :: ClientM [ItemInfo]
search :: Text -> ClientM [ItemInfo]
stockQuantity :: ItemKey -> ClientM Quantity
restock :: AdminAction_RestockBody -> ClientM NoContent
addItem :: AdminAction_AddItemBody -> ClientM ItemKey
removeItem :: AdminAction_RemoveItemBody -> ClientM NoContent

buyItem :<|> listItems :<|> search :<|> stockQuantity :<|> (restock :<|> addItem :<|> removeItem)
    = client $ Proxy @StoreActionApi


type ExpectedReverseText
    = "ReverseText" :> ReqBody '[PlainText] Text :> Post '[JSON] Text

$(mkServer testActionConfig ''TestAction)

expectedReverseText :: Text -> ClientM Text
expectedReverseText = client (Proxy @ExpectedReverseText)

withStoreServer :: IO () -> IO ()
withStoreServer runTests = do
    p      <- createForgetful applyStoreEvent mempty
    -- server <- async . run 9898 $ serve (Proxy @StoreActionApi) undefined
    server <- async . run 9898 $ serve
        (Proxy @StoreActionApi)
        (storeActionServer $ runAction p handleStoreAction)
    threadDelay 10000 -- Ensure the server is live when the tests run
    runTests
    cancel server

withTestServer :: IO () -> IO ()
withTestServer runTests = do
    p      <- createForgetful (\m _ -> m) ()
    -- server <- async . run 9898 $ serve (Proxy @StoreActionApi) undefined
    server <- async . run 9898 $ serve
        (Proxy @TestActionApi)
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
