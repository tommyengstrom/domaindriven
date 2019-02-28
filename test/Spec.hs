import           RIO
import qualified RIO.List as L
import DomainDriven
import Data.Aeson
import Data.Generics.Product
import GHC.TypeLits
import GHC.Enum
import qualified Data.Map as M

newtype Wrap (s :: Symbol) a = Wrap {unWrap :: a}
    deriving newtype (Show, Eq, Ord, FromJSON, ToJSON)
    deriving stock (Generic, Functor)

type ItemKey = Wrap "ItemKey" Int

data StoreCmd
    = BuyItem ItemKey Int
    | Restock ItemKey Int
    | AddItem ItemInfo
    | RemoveItem ItemKey
    deriving stock (Show, Eq, Ord, Generic)

data StoreEvent
    = BoughtItem ItemKey Int
    | Restocked ItemKey Int
    | AddedItem ItemKey ItemInfo
    | RemovedItem ItemKey

data ItemInfo = ItemInfo
    { quantity :: Int
    , price :: Int
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Err
    = NotEnoughStock
    | NoSuchItem
    deriving (Show, Eq, Ord, Typeable)
instance Exception Err

type StoreModel = Map ItemKey ItemInfo

instance DomainModel StoreModel StoreCmd StoreEvent where
    initial = mempty
    applyEvent m e = case storedEvent e of
        BoughtItem iKey q ->
            M.update (Just . over (field @"quantity") (\x -> x-q)) iKey m
        Restocked iKey q ->
            M.update (Just . over (field @"quantity") (+ q)) iKey m
        AddedItem iKey info ->
            M.insert iKey info m
        RemovedItem iKey ->
            M.delete iKey m
    evalCmd m = \case
        BuyItem iKey q -> do
            let available = maybe 0 (^. field @"quantity") $ M.lookup iKey m
            when (available < q) $ throwM NotEnoughStock
            pure [BoughtItem iKey q]
        Restock iKey q -> do
            when (M.notMember iKey m) $ throwM NoSuchItem
            pure [Restocked iKey q]
        AddItem info -> do
            let iKey = succ <$> fromMaybe (Wrap 0) (L.maximumMaybe $ M.keys m)
            pure [AddedItem iKey info]
        RemoveItem iKey -> do
            when (M.notMember iKey m) $ throwM NoSuchItem
            pure [RemovedItem iKey]

main :: IO ()
main = error "Test suite not yet implemented"
