module DomainDriven.Persistance.ForgetfulSTMSpec where
import           Prelude
import           DomainDriven.Internal.Class
import qualified Data.Map                                     as M
import           Test.Hspec
import           Safe                           ( headNote )
import           DomainDriven.Persistance.ForgetfulSTM
import           StoreModel

spec :: Spec
spec = describe "Test basic functionality" $ do
    es <- runIO $ createForgetfulSTM applyStoreEvent mempty


    it "Can add item" $ do
        let item :: ItemInfo
            item = ItemInfo 10 49
        iKey <- runCmd es handleStoreCmd $ AddItem item
        getModel es `shouldReturn` M.singleton iKey item

    it "Can buy item" $ do
        iKey <- headNote "Ops" . M.keys <$> getModel es
        runCmd es handleStoreCmd $ BuyItem iKey 7
        getModel es `shouldReturn` M.singleton (Wrap 1) (ItemInfo 3 49)


    it "Can run Query" $ do
        runQuery es queryHandler ProductCount `shouldReturn` 1

        let item :: ItemInfo
            item = ItemInfo 4 33
        _ <- runCmd es handleStoreCmd $ AddItem item

        runQuery es queryHandler ProductCount `shouldReturn` 2
