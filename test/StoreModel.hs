module StoreModel where


import qualified RIO.List                                     as L
import qualified Data.Map                                     as M
import           DomainDriven
import           RIO
import           Data.Aeson
import           GHC.TypeLits
import           Data.Generics.Product
import           Control.Monad.Except
import           GHC.Enum

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

data StoreQuery a where
    ProductCount ::StoreQuery Int

queryHandler :: StoreModel -> StoreQuery a -> IO (Either StoreError a)
queryHandler m = \case
    ProductCount ->
        pure . Right . length . M.keys $ M.filter ((> 0) . view (typed @Quantity)) m
