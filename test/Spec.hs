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

-- The commands
data BuyItem    = BuyItem ItemKey Quantity
data Restock    = Restock ItemKey Quantity
data AddItem    = AddItem ItemInfo
data RemoveItem = RemoveItem ItemKey

type StoreCmds = Command BuyItem ()
             :|| Command Restock ()
             :|| Command AddItem ItemKey
             :|| Command RemoveItem ()

buyItem :: BuyItem -> ReaderT (StmState StoreModel) IO (StoreEvent, ())
buyItem (BuyItem iKey q) = do
    m <- readTVarIO =<< asks currentState
    let available = maybe 0 (^. field @"quantity") $ M.lookup iKey m
    when (available < q) $ throwM NotEnoughStock
    pure (BoughtItem iKey q, ())


restock :: Restock -> ReaderT (StmState StoreModel) IO (StoreEvent, ())
restock (Restock iKey q) = do
    m <- readTVarIO =<< asks currentState
    when (M.notMember iKey m) $ throwM NoSuchItem
    pure (Restocked iKey q, ())

addItem :: AddItem -> ReaderT (StmState StoreModel) IO (StoreEvent, ItemKey)
addItem (AddItem info) = do
    m <- readTVarIO =<< asks currentState
    let iKey = succ <$> fromMaybe (Wrap 0) (L.maximumMaybe $ M.keys m)
    pure (AddedItem iKey info, iKey)

removeItem :: RemoveItem -> ReaderT (StmState StoreModel) IO (StoreEvent, ())
removeItem (RemoveItem iKey) = do
    m <- readTVarIO =<< asks currentState
    when (M.notMember iKey m) $ throwM NoSuchItem
    pure (RemovedItem iKey, ())

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

runTestRunner :: (EventSourced m, m ~ StoreModel)
              => ReaderT (StmState m) IO a -> IO a
runTestRunner m = do
    s <- mkState
    runReaderT m s

mkState :: IO (StmState StoreModel)
mkState = do
    tvar <- newTVarIO mempty
    pure $ StmState
        { writeEvent   = const $ pure ()
        , readEvents   = pure []
        , currentState = tvar
        }

instance EventSourced StoreModel where
    type Event StoreModel = StoreEvent
    type Cmds StoreModel = StoreCmds
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
        tvar <- asks currentState
        atomically $ modifyTVar tvar f

    cmdHandlers = buyItem :|| restock :|| addItem :|| removeItem

main :: IO ()
main = hspec . describe "Store model" $ do
    it "Can add item" $ do
        r <- runTestRunner $ do
            _ <- runCmd $ AddItem (ItemInfo 10 49)
            s <- asks currentState
            readTVarIO s
        r `shouldBe` M.singleton (Wrap 1) (ItemInfo 10 49)

    it "Can add item and buy it" $ do
        r <- runTestRunner $ do
            iKey <- runCmd  $ AddItem (ItemInfo 10 49)
            _ <- runCmd $ BuyItem iKey 7
            readTVarIO =<< asks currentState
        r `shouldBe` M.singleton (Wrap 1) (ItemInfo 3 49)

