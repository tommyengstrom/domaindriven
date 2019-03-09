import           RIO
import qualified RIO.List as L
import DomainDriven
import Data.Aeson
import Data.Generics.Product
import GHC.TypeLits
import GHC.Enum
import qualified Data.Map as M
import Test.Hspec

newtype Wrap (s :: Symbol) a = Wrap {unWrap :: a}
    deriving newtype (Show, Eq, Ord, FromJSON, ToJSON, Num)
    deriving stock (Generic, Functor)

type ItemKey = Wrap "ItemKey" Int
type Quantity = Wrap "Quantity" Int

data StoreCmd a where
    BuyItem :: ItemKey -> Quantity -> StoreCmd ()
    Restock :: ItemKey -> Quantity -> StoreCmd ()
    AddItem :: ItemInfo -> StoreCmd ItemKey
    RemoveItem :: ItemKey -> StoreCmd ()

data StoreEvent
    = BoughtItem ItemKey Quantity
    | Restocked ItemKey Quantity
    | AddedItem ItemKey ItemInfo
    | RemovedItem ItemKey
    deriving stock (Show, Eq, Ord, Generic)

data ItemInfo = ItemInfo
    { quantity :: Quantity
    , price :: Int
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Err
    = NotEnoughStock
    | NoSuchItem
    deriving (Show, Eq, Ord, Typeable)
instance Exception Err

type StoreModel = Map ItemKey ItemInfo
data Apa = Apa
    { model :: StoreModel
    , events :: [Stored StoreEvent]
    } deriving (Show, Eq, Ord, Generic)

newtype TestRunner a
    = TestRunner (ReaderT (TVar Apa) IO a)
    deriving newtype ( Functor, Applicative, Monad, MonadReader (TVar Apa), MonadIO
                     , MonadThrow)

runTestRunner :: TestRunner a -> IO a
runTestRunner (TestRunner m) = do
    tvar <- newTVarIO $ Apa mempty mempty
    runReaderT m tvar

instance ESRunner TestRunner where
    type Event TestRunner = StoreEvent
    type Cmd TestRunner = StoreCmd
    type Model TestRunner = StoreModel
    readEvents = fmap (view (field @"events")) . readTVarIO =<< ask
    applyEvent e = do
        let f = case storedEvent e of
                BoughtItem iKey q ->
                    M.update (Just . over (field @"quantity") (\x -> x-q)) iKey
                Restocked iKey q ->
                    M.update (Just . over (field @"quantity") (+ q)) iKey
                AddedItem iKey info ->
                    M.insert iKey info
                RemovedItem iKey ->
                    M.delete iKey
        tvar <- ask
        atomically . modifyTVar tvar $ over (field @"model") f
    persistEvent e = do
        s <- toStored e -- :: TestRunner (Stored StoreEvent)
        a <- ask
        atomically . modifyTVar a $ over (field @"events") (s:)
        pure s
    evalCmd cmd = do
        m <- fmap (view (field @"model")) . readTVarIO @_ @Apa =<< ask
        case cmd of
            BuyItem iKey q -> do
                let available = maybe 0 (^. field @"quantity") $ M.lookup iKey m
                when (available < q) $ throwM NotEnoughStock
                pure (BoughtItem iKey q, ())
            Restock iKey q -> do
                when (M.notMember iKey m) $ throwM NoSuchItem
                pure (Restocked iKey q, ())
            AddItem info -> do
                let iKey = succ <$> fromMaybe (Wrap 0) (L.maximumMaybe $ M.keys m)
                pure (AddedItem iKey info, iKey)
            RemoveItem iKey -> do
                when (M.notMember iKey m) $ throwM NoSuchItem
                pure (RemovedItem iKey, ())


main :: IO ()
main = hspec . describe "Store model" $ do
    it "Can add item" $ do
        r <- runTestRunner $ do
            _ <- runCmd (AddItem (ItemInfo 10 49))
            readTVarIO =<< ask
        view (field @"model") r
            `shouldBe` M.singleton (Wrap 1) (ItemInfo 10 49)

    it "Can add item and buy it" $ do
        r <- runTestRunner $ do
            iKey <- runCmd  $ AddItem (ItemInfo 10 49)
            _ <- runCmd $ BuyItem iKey 7
            readTVarIO =<< ask
        view (field @"model") r
            `shouldBe` M.singleton (Wrap 1) (ItemInfo 3 49)

