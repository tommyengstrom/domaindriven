import           RIO
import qualified RIO.List                      as L
import           DomainDriven
import           Data.Aeson
import           Data.Generics.Product
import           GHC.TypeLits
import           GHC.Enum
import qualified Data.Map                      as M
import           Test.Hspec
import           Data.UUID.V1
import           Data.UUID                      ( nil )
import           RIO.Time

newtype Wrap (s :: Symbol) a = Wrap {unWrap :: a}
    deriving newtype (Show, Eq, Ord, FromJSON, ToJSON, Num)
    deriving stock (Generic, Functor)

type ItemKey = Wrap "ItemKey" Int
type Quantity = Wrap "Quantity" Int

-- The commands
data BuyItem    = BuyItem ItemKey Quantity
data Restock    = Restock ItemKey Quantity
data AddItem    = AddItem ItemInfo
data RemoveItem = RemoveItem ItemKey


--instance Command BuyItem StoreModel () where
--    type Deps StoreModel = ()
--    cmdHandler _ tvar (BuyItem iKey q) = do
--        m <- readTVarIO tvar
--        let available = maybe 0 (^. field @"quantity") $ M.lookup iKey m
--        when (available < q) $ throwM NotEnoughStock
--        pure (BoughtItem iKey q, ())

instance Command BuyItem StoreModel where
    type CmdDeps BuyItem = ()
    type CmdReturn BuyItem = ()
    cmdHandler _ tvar (BuyItem iKey q) = do
        m <- readTVarIO tvar
        let available = maybe 0 (^. field @"quantity") $ M.lookup iKey m
        when (available < q) $ throwM NotEnoughStock
        pure (BoughtItem iKey q, ())


instance Command Restock StoreModel where
    type CmdDeps Restock = ()
    type CmdReturn Restock = ()
    cmdHandler _ tvar (Restock iKey q) = do
        m <- readTVarIO tvar
        when (M.notMember iKey m) $ throwM NoSuchItem
        pure (Restocked iKey q, ())

instance Command AddItem StoreModel where
    type CmdDeps AddItem = ()
    type CmdReturn AddItem = ItemKey
    cmdHandler _ tvar (AddItem info) = do
        m <- readTVarIO tvar
        let iKey = succ <$> fromMaybe (Wrap 0) (L.maximumMaybe $ M.keys m)
        pure (AddedItem iKey info, iKey)

instance Command RemoveItem StoreModel where
    type CmdDeps RemoveItem = ()
    type CmdReturn RemoveItem = ()
    cmdHandler _ tvar (RemoveItem iKey) = do
        m <- readTVarIO tvar
        when (M.notMember iKey m) $ throwM NoSuchItem
        pure (RemovedItem iKey, ())

-- Queries
data ProductCount = ProductCount

instance Query ProductCount where
    type QueryDeps ProductCount = StoreModel
    type QueryReturn ProductCount = Int
    runQuery getModel ProductCount = do
        m <- getModel ProductCount
        pure . length $ M.elems $ M.filter ((> Wrap 0) . view (typed @Quantity)) m

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

runTestRunner :: (EventSourced m, m ~ StoreModel) => ReaderT (TVar m) IO a -> IO a
runTestRunner m = do
    s <- newTVarIO mempty
    runReaderT m s

instance EventSourced StoreModel where
    type Event StoreModel = StoreEvent
    readEvents = pure [] -- nothing is persisted in the tests
    applyEvent tvar e = do
        let f = case storedEvent e of
                BoughtItem iKey q ->
                    M.update (Just . over (field @"quantity") (\x -> x - q)) iKey
                Restocked iKey q -> M.update (Just . over (field @"quantity") (+ q)) iKey
                AddedItem iKey info -> M.insert iKey info
                RemovedItem iKey -> M.delete iKey
        atomically $ modifyTVar tvar f

    persistEvent e = do
        ts   <- getCurrentTime
        uuid <- fromMaybe nil <$> liftIO nextUUID
        pure $ Stored e ts uuid


main :: IO ()
main = hspec . describe "Store model" $ do
    it "Can add item" $ do
        r <- do
            tvar <- newTVarIO mempty
            _    <- runCmd (const $ pure ()) tvar $ AddItem (ItemInfo 10 49)
            readTVarIO tvar
        r `shouldBe` M.singleton (Wrap 1) (ItemInfo 10 49)

    it "Can add item and buy it" $ do
        r <- do
            tvar <- newTVarIO mempty
            iKey <- runCmd (const $ pure ()) tvar $ AddItem (ItemInfo 10 49)
            runCmd (const $ pure ()) tvar $ BuyItem iKey 7
            readTVarIO tvar
        r `shouldBe` M.singleton (Wrap 1) (ItemInfo 3 49)

    it "Can run Query" $ do
        r <- do
            tvar <- newTVarIO mempty
            _    <- runCmd (const $ pure ()) tvar $ AddItem (ItemInfo 10 49)
            _    <- runCmd (const $ pure ()) tvar $ AddItem (ItemInfo 1 732)
            _    <- runCmd (const $ pure ()) tvar $ AddItem (ItemInfo 22 14)
            runQuery (const $ readTVarIO tvar) ProductCount
        r `shouldBe` 3
