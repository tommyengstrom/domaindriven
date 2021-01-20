module DomainDriven.Persistance.FileWithSTMSpec where

import           Prelude
import           Control.Monad
import           Control.Concurrent
import           DomainDriven.Internal.Class
import qualified Data.Map                                     as M
import           Test.Hspec
import           System.Directory
import           System.Mem
import           DomainDriven.Persistance.FileWithSTM
import           StoreModel

spec :: Spec
spec = describe "Test basic event persistance" $ do

    let fp = "/tmp/persisted-model.events"
    it "File storage works" $ do
        fileExists <- doesFileExist fp
        when fileExists $ removeFile fp
        esp <- createFileWithSTM fp applyStoreEvent mempty
        let item :: ItemInfo
            item = ItemInfo 32 7
        iKey <- runCmd esp handleStoreCmd $ AddItem item
        getModel esp `shouldReturn` M.singleton iKey item

    it "File storage rembers" $ do
        performMajorGC
        threadDelay 100  -- Meh, this is awkward. Fix it sometime!
        esp <- createFileWithSTM fp applyStoreEvent mempty
        m   <- getModel esp
        m `shouldSatisfy` (== 1) . M.size
