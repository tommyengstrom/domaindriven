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

data StoreCmd
    = BuyItem ItemKey Quantity
    | Restock ItemKey Quantity
    | AddItem ItemInfo
    | RemoveItem ItemKey
    deriving stock (Show, Eq, Ord, Generic)

-- can I use a GADT for the command and in it specify what it should return?
data StoreCmd' a where
    BuyItem' :: ItemKey -> Quantity -> StoreCmd' ()
    Restock' :: ItemKey -> Quantity -> StoreCmd' ()
    AddItem' :: ItemInfo -> StoreCmd' ItemKey

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

-- Things to figure out
-- * How should I go about generating the client and server?
-- * How can I make a nice abstraction that allows CQRS with or without event sourcing?
--
----- How to generate the server and client
--
-- * Every constructor of the command corresponds to an endpoint
-- * The name of the constructor is the name of the endpoint
-- * All parameters of the constructor are contained in the request body
-- * the type of the parameter is used for the name. e.g.
-- `BuyItem 1 10` would be:
-- {Tag: BuyItem, ItemKey: Wrap 1, Quantity: Wrap 10}
-- * I need a type level association between command and event in order to be able to
-- return the right thing in my generated model

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
            pure $ BoughtItem iKey q
        Restock iKey q -> do
            when (M.notMember iKey m) $ throwM NoSuchItem
            pure $ Restocked iKey q
        AddItem info -> do
            let iKey = succ <$> fromMaybe (Wrap 0) (L.maximumMaybe $ M.keys m)
            pure $ AddedItem iKey info
        RemoveItem iKey -> do
            when (M.notMember iKey m) $ throwM NoSuchItem
            pure $ RemovedItem iKey

main :: IO ()
main = hspec . describe "Store model" $ do
    it "Can add item" $ do
        applyEvent (mempty :: StoreModel) (Stored (AddedItem (Wrap 1) (ItemInfo 10 49)) undefined undefined)
            `shouldBe` M.singleton (Wrap 1) (ItemInfo 10 49)

