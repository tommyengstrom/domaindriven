{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.ServerSpec where

import           DomainDriven.Server
import           Prelude
import           GHC.Generics                   ( Generic )
import           Test.Hspec
import           Data.Aeson
import           Control.Concurrent.Async
import           DomainDriven.Persistance.ForgetfulInMemory
import           DomainDriven
import           Servant
import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           Data.OpenApi                   ( ToSchema )
import           Servant.Client
import           DomainDriven.ServerOptions
import           StoreModel
import           Data.Text                      ( Text )


-- | Compare with StoreActionApi if you run into issues
--type ExpectedStoreActionApi
--    = "cart" :> "add" :> ReqBody '[JSON] (NamedFields1 "Add" ItemKey) :> Post '[JSON] NoContent
--    :<|> "Int" :> Capture "Int" Int :> "itemaction" :> "subop1" :> Post '[JSON] NoContent
--
--cartAdd :: NamedFields1 "Add" ItemKey -> ClientM NoContent
--itemSubOp1 :: Int -> ClientM NoContent
--cartAdd :<|> itemSubOp1 = client (Proxy @StoreActionApi)

buyItem :: NamedFields2 "BuyItem" ItemKey Quantity -> ClientM NoContent
listItems :: ClientM [ItemInfo]
search :: NamedFields1 "Search" Text -> ClientM [ItemInfo]
stockQuantity :: ItemKey -> ClientM Quantity
restock :: NamedFields2 "AdminAction_Restock" ItemKey Quantity -> ClientM NoContent
addItem :: NamedFields3 "AdminAction_AddItem" ItemName Quantity Price -> ClientM ItemKey
removeItem :: NamedFields1 "AdminAction_RemoveItem" ItemKey -> ClientM NoContent

-- Damn, I need to foldr. This is silly!
(((buyItem :<|> listItems) :<|> search) :<|> stockQuantity) :<|> ((restock :<|> addItem) :<|> removeItem)
    = client $ Proxy @StoreActionApi




--spec :: Spec
--spec = do
--    runIO $ do
--        p <- createForgetful applyStoreEvent mempty
--        _ <- async . run 9898 $ serve (Proxy @StoreActionApi)
--                                      (storeActionServer $ runCmd p handleStoreAction)
--        pure ()
--    clientEnv <- runIO $ do
--        man <- newManager defaultManagerSettings
--        pure $ mkClientEnv man (BaseUrl Http "localhost" 9898 "")
--    describe "Server endpoint renaming" $ do
--        it "Can split constructor into multiple path segments" $ do
--            r <- runClientM (cartAdd $ NamedFields1 $ ItemKey "whatever") clientEnv
--            r `shouldBe` Right NoContent
--        it "Can remove constructor name" $ do
--            r <- runClientM (itemSubOp1 1) clientEnv
--            r `shouldBe` Right NoContent
