{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.ServerSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.Catch            ( try )
import           Control.Monad.Except
import           Data.Aeson                     ( encode )
import qualified Data.ByteString.Lazy.Char8                   as BL
import           Data.Text                      ( Text )
import           DomainDriven
import           DomainDriven.Persistance.ForgetfulInMemory
import           DomainDriven.Server
import           DomainDriven.ServerSpecModel
import           Models.Store
import           Network.HTTP.Client            ( defaultManagerSettings
                                                , newManager
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           Prelude
import           Servant
import           Servant.API.Flatten
import           Servant.Client
import           Servant.OpenApi                ( toOpenApi )
import           Test.Hspec
-- import           Test.Hspec.Core.Hooks

-- $(mkServer storeActionConfig ''StoreAction)
--
-- buyItem :: NamedFields2 "StoreAction_BuyItem" ItemKey Quantity -> ClientM NoContent
-- listItems :: ClientM [ItemInfo]
-- search :: Text -> ClientM [ItemInfo]
-- itemPrice :: ItemKey -> ClientM Price
-- itemStockQuantity :: ItemKey -> ClientM Quantity
--
-- adminOrder :: NamedFields2 "AdminAction_Order" ItemKey Quantity -> ClientM NoContent
-- adminRestock :: NamedFields2 "AdminAction_Restock" ItemKey Quantity -> ClientM NoContent
-- adminAddItem
--     :: NamedFields3 "AdminAction_AddItem" ItemName Quantity Price -> ClientM ItemKey
-- adminRemoveItem :: NamedFields1 "AdminAction_RemoveItem" ItemKey -> ClientM NoContent
--
-- buyItem :<|> listItems :<|> search :<|> itemStockQuantity :<|> itemPrice :<|> adminOrder :<|> adminRestock :<|> adminAddItem :<|> adminRemoveItem
--     = client (flatten $ Proxy @StoreActionApi)
--
--
-- type ExpectedReverseText
--     = "ReverseText" :> ReqBody '[PlainText] Text :> Post '[JSON] Text
--
-- -- type ExpectedConcatText
-- --     = "ConcatText"
-- --     :> QueryParam' '[Strict, Required] "Text" Text
-- --     :> QueryParam' '[Strict, Required] "Text_1" Text
-- --     :> Get '[JSON] Text
--
-- type ExpectedIntersperse
--     = "SubAction"
--     :> QueryParam' '[Strict, Required] "text" Text
--     :> "Intersperse"
--     :> QueryParam' '[Strict, Required] "char" Char
--     :> Get '[JSON] Text

$(mkServer (ServerConfig mempty mempty) ''TestAction)

expectedReverseText :: Text -> ClientM Text
expectedReverseText = client (Proxy @ExpectedReverseText)

-- expectedConcatText :: Text -> Text -> ClientM Text
-- expectedConcatText = client (Proxy @ExpectedConcatText)

expectedIntersperseText :: Text -> Char -> ClientM Text
expectedIntersperseText = client (Proxy @ExpectedIntersperse)

writeOpenApi :: IO ()
writeOpenApi =
    BL.writeFile "/tmp/store_schema.json" $ encode $ toOpenApi (Proxy @StoreActionApi)

withStoreServer :: IO () -> IO ()
withStoreServer runTests = do
    p      <- createForgetful applyStoreEvent mempty
    server <- async . run 9898 $ serve (Proxy @StoreActionApi) $ hoistServer
        (Proxy @StoreActionApi)
        (Handler . ExceptT . try)
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
        (Handler . ExceptT . try)
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
                r <- runClientM (adminAddItem $ NamedFields3 "Test item" 10 99) clientEnv
                r `shouldSatisfy` not . null
            it "The new item shows up when listing items" $ do
                r <- runClientM listItems clientEnv
                case r of
                    Right [ItemInfo _ n _ _ _] -> n `shouldBe` "Test item"
                    a -> fail $ "That shouldn't happen! " <> show a
    aroundAll_ withTestServer $ do
        describe "Endpoints generated as expected" $ do
            it "Plaintext endpoint works" $ do
                r <- runClientM (expectedReverseText "Hej") clientEnv
                r `shouldBe` Right "jeH"
            it "Produces the expected parameters for subserver" $ do
                r <- runClientM (expectedIntersperseText "hello" '-') clientEnv
                r `shouldBe` Right "h-e-l-l-o"
