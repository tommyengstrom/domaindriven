{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.ServerSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.Catch            ( try )
import           Control.Monad.Except
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
import           Test.Hspec
-- import           Test.Hspec.Core.Hooks

$(mkServer storeActionConfig ''StoreAction)
--
itemBuy :: ItemKey -> NF1 "ItemAction_Buy" "quantity" Quantity -> ClientM NoContent
listItems :: ClientM [ItemInfo]
search :: Text -> ClientM [ItemInfo]
itemPrice :: ItemKey -> ClientM Price
itemStockQuantity :: ItemKey -> ClientM Quantity

adminOrder
    :: NF2 "AdminAction_Order" "item" ItemKey "quantity" Quantity -> ClientM NoContent
adminRestock
    :: NF2 "AdminAction_Restock" "itemKey" ItemKey "quantity" Quantity
    -> ClientM NoContent
adminAddItem
    :: NF3 "AdminAction_AddItem" "itemName" ItemName "quantity" Quantity "price" Price
    -> ClientM ItemKey
adminRemoveItem :: NF1 "AdminAction_RemoveItem" "item" ItemKey -> ClientM NoContent
--
listItems :<|> search :<|> itemBuy :<|> itemStockQuantity :<|> itemPrice :<|> adminOrder :<|> adminRestock :<|> adminAddItem :<|> adminRemoveItem
    = client (flatten $ Proxy @StoreActionApi)
--
--
type ExpectedReverseText
    = "ReverseText" :> ReqBody '[JSON] (NF1 "a" "text" Text) :> Post '[JSON] Text

--type ExpectedConcatText
--    = "ConcatText"
--    :> QueryParam' '[Strict, Required] "Text" Text
--    :> QueryParam' '[Strict, Required] "Text_1" Text
--    :> Get '[JSON] Text

type ExpectedIntersperse
    = "Sub"
    :> QueryParam' '[Strict, Required] "text" Text
    :> "Intersperse"
    :> QueryParam' '[Strict, Required] "intersperse_text" Text
    :> Get '[JSON] Text

$(mkServer testActionConfig ''TestAction)

expectedReverseText :: NF1 "a" "text" Text -> ClientM Text
expectedReverseText = client (Proxy @ExpectedReverseText)

-- expectedConcatText :: Text -> Text -> ClientM Text
-- expectedConcatText = client (Proxy @ExpectedConcatText)

expectedIntersperseText :: Text -> Text -> ClientM Text
expectedIntersperseText = client (Proxy @ExpectedIntersperse)

--writeOpenApi :: IO ()
--writeOpenApi =
--    BL.writeFile "/tmp/store_schema.json" $ encode $ toOpenApi (Proxy @StoreActionApi)
--
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
        (testActionServer $ runAction p handleAction)
    threadDelay 10000 -- Ensure the server is live when the tests run
    runTests
    cancel server


kuken :: IO ()
kuken = do
    p <- createForgetful (\m _ -> m) ()
    run 9898 $ serve (Proxy @TestActionApi) $ hoistServer
        (Proxy @TestActionApi)
        (Handler . ExceptT . try)
        (testActionServer $ runAction p handleAction)

spec :: Spec
spec = do
    clientEnv <- runIO $ do
        man <- newManager defaultManagerSettings
        pure $ mkClientEnv man (BaseUrl Http "localhost" 9898 "")
    aroundAll_ withStoreServer $ do

        describe "Server endpoint renaming" $ do
            it "Can add item" $ do
                r <- runClientM (adminAddItem $ NF3 "Test item" 10 99) clientEnv
                r `shouldSatisfy` not . null
            it "The new item shows up when listing items" $ do
                r <- runClientM listItems clientEnv
                case r of
                    Right [ItemInfo _ n _ _ _] -> n `shouldBe` "Test item"
                    a -> fail $ "That shouldn't happen! " <> show a
    aroundAll_ withTestServer $ do
        describe "Endpoints generated as expected" $ do
            it "Plaintext endpoint works" $ do
                r <- runClientM (expectedReverseText $ NF1 "Hej") clientEnv
                r `shouldBe` Right "jeH"
            it "Produces the expected parameters for subserver" $ do
                r <- runClientM (expectedIntersperseText "hello" "-=") clientEnv
                r `shouldBe` Right "h=-=e=-=l=-=l=-=o"
