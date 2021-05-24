{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.ServerSpec where

import           DomainDriven.Server
import           Prelude
import           Test.Hspec
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
import           StoreModel
import           Data.Text                      ( Text )

$(mkServer domaindrivenServerConfig ''StoreAction)

buyItem :: NamedFields2 "StoreAction_BuyItem" ItemKey Quantity -> ClientM NoContent
listItems :: ClientM [ItemInfo]
search :: Text -> ClientM [ItemInfo]
stockQuantity :: ItemKey -> ClientM Quantity
restock :: NamedFields2 "AdminAction_Restock" ItemKey Quantity -> ClientM NoContent
addItem :: NamedFields3 "AdminAction_AddItem" ItemName Quantity Price -> ClientM ItemKey
removeItem :: NamedFields1 "AdminAction_RemoveItem" ItemKey -> ClientM NoContent

buyItem :<|> listItems :<|> search :<|> stockQuantity :<|> (restock :<|> addItem :<|> removeItem)
    = client $ Proxy @StoreActionApi


withServer :: IO () -> IO ()
withServer runTests = do
    p      <- createForgetful applyStoreEvent mempty
    -- server <- async . run 9898 $ serve (Proxy @StoreActionApi) undefined
    server <- async . run 9898 $ serve
        (Proxy @StoreActionApi)
        (storeActionServer $ runAction p handleStoreAction)
    threadDelay 10000 -- Ensure the server is live when the tests run
    runTests
    cancel server

spec :: Spec
spec = aroundAll_ withServer $ do

    clientEnv <- runIO $ do
        man <- newManager defaultManagerSettings
        pure $ mkClientEnv man (BaseUrl Http "localhost" 9898 "")
    describe "Server endpoint renaming" $ do
        it "Can add item" $ do
            r <- runClientM (addItem $ NamedFields3 "Test item" 10 99) clientEnv
            r `shouldSatisfy` not . null
        it "The new item shows up when listing items" $ do
            r <- runClientM listItems clientEnv
            case r of
                Right [ItemInfo _ n _ _] -> n `shouldBe` "Test item"
                a                        -> fail $ "That shouldn't happen! " <> show a
