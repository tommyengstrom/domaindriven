import           RIO
import qualified RIO.List                                     as L
import           DomainDriven.Internal.Class
import           Data.Aeson
import           Data.Generics.Product
import           GHC.TypeLits
import           GHC.Enum
import qualified Data.Map                                     as M
import           Test.Hspec
import           Safe                           ( headNote )
import           Control.Monad.Except
import           System.Directory
import           System.Mem
import           DomainDriven.Persistance.ForgetfulSTM
-- import           DomainDriven.Persistance.FileAndSTM

newtype Wrap (s :: Symbol) a = Wrap {unWrap :: a}
    deriving newtype (Show, Eq, Ord, FromJSON, ToJSON, Num)
    deriving stock (Generic, Functor)

type ItemKey = Wrap "ItemKey" Int
type Quantity = Wrap "Quantity" Int

-- Command
data StoreCmd a where
    BuyItem    ::ItemKey -> Quantity -> StoreCmd ()
    Restock    ::ItemKey -> Quantity -> StoreCmd ()
    AddItem    ::ItemInfo -> StoreCmd ItemKey
    RemoveItem ::ItemKey -> StoreCmd ()

data StoreEvent
    = BoughtItem ItemKey Quantity
    | Restocked ItemKey Quantity
    | AddedItem ItemKey ItemInfo
    | RemovedItem ItemKey
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ItemInfo = ItemInfo
    { quantity :: Quantity
    , price    :: Int
    }
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data StoreError
    = NotEnoughStock
    | NoSuchItem
    deriving (Show, Eq, Ord, Typeable, Exception)

type StoreModel = Map ItemKey ItemInfo

-- handleStoreCmd :: StoreCmd a -> IO (StoreModel -> Either StoreError (a, [StoreEvent]))
handleStoreCmd :: CmdHandler StoreModel StoreEvent StoreCmd StoreError
handleStoreCmd = \case
    BuyItem iKey q -> pure $ \m -> runExcept $ do
        let available = maybe 0 (^. field @"quantity") $ M.lookup iKey m
        when (available < q) $ throwError NotEnoughStock
        pure ((), [BoughtItem iKey q])
    Restock iKey q -> pure $ \m -> runExcept $ do
        when (M.notMember iKey m) $ throwError NoSuchItem
        pure ((), [Restocked iKey q])
    AddItem iInfo -> pure $ \m -> runExcept $ do
        let iKey = succ <$> fromMaybe (Wrap 0) (L.maximumMaybe $ M.keys m)
        pure (iKey, [AddedItem iKey iInfo])
    RemoveItem iKey -> pure $ \m -> runExcept $ do
        when (M.notMember iKey m) $ throwError NoSuchItem
        pure ((), [RemovedItem iKey])

applyStoreEvent :: StoreModel -> Stored StoreEvent -> StoreModel
applyStoreEvent m (Stored e _ _) = case e of
    BoughtItem iKey q -> M.update (Just . over (field @"quantity") (\x -> x - q)) iKey m
    Restocked  iKey q    -> M.update (Just . over (field @"quantity") (+ q)) iKey m
    AddedItem  iKey info -> M.insert iKey info m
    RemovedItem iKey     -> M.delete iKey m




-- mkPersistedModel
--     :: FilePath
--     -> IO (DomainModel (FileAndSTM StoreModel StoreEvent) StoreModel StoreEvent)
-- mkPersistedModel fp = do
--     createFileAndSTM fp applyStoreEvent mempty

data StoreQuery a where
    ProductCount ::StoreQuery Int

queryHandler :: StoreModel -> StoreQuery a -> IO (Either StoreError a)
queryHandler m = \case
    ProductCount ->
        pure . Right . length . M.keys $ M.filter ((> 0) . view (typed @Quantity)) m


-- getFileModel
--     :: DomainModel (FileAndSTM StoreModel StoreEvent) model event
--     -> IO (Map ItemKey ItemInfo)
-- getFileModel = getModel . persistanceHandler

main :: IO ()
main = hspec . describe "Store model" $ do
    es <- runIO $ createForgetfulSTM applyStoreEvent mempty
    let fp = "/tmp/persisted-model.events"


    it "Can add item" $ do
        let item :: ItemInfo
            item = ItemInfo 10 49
        iKey <- runCmd es handleStoreCmd $ AddItem item
        getModel (persistanceHandler es) `shouldReturn` M.singleton iKey item

    it "Can buy item" $ do
        iKey <- headNote "Ops" . M.keys <$> getModel (persistanceHandler es)
        runCmd es handleStoreCmd $ BuyItem iKey 7
        r <- getModel $ (persistanceHandler es)
        r `shouldBe` M.singleton (Wrap 1) (ItemInfo 3 49)


    it "Can run Query" $ do
        runQuery es queryHandler ProductCount `shouldReturn` 1

        let item :: ItemInfo
            item = ItemInfo 4 33
        _ <- runCmd es handleStoreCmd $ AddItem item

        runQuery es queryHandler ProductCount `shouldReturn` 2
--
--    it "File storage works" $ do
--        fileExists <- doesFileExist fp
--        when fileExists $ removeFile fp
--        esp <- mkPersistedModel fp
--        let item :: ItemInfo
--            item = ItemInfo 32 7
--        iKey <- runCmd esp handleStoreCmd $ AddItem item
--        getFileModel esp `shouldReturn` M.singleton iKey item
--
--    it "File storage rembers" $ do
--        performMajorGC
--        threadDelay 100  -- Meh, this is bullshit. Fix it sometime!
--        esp <- mkPersistedModel fp
--        m   <- getFileModel esp
--        m `shouldSatisfy` (== 1) . M.size
