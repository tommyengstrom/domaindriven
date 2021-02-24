module StoreModel where


import qualified Data.Map                                     as M
import           DomainDriven
import           Prelude
import           Data.Aeson
import           GHC.TypeLits
import           Data.Generics.Product
import           Control.Monad.Catch
import           Safe                           ( maximumMay )
import           Data.Typeable
import           Data.Map                       ( Map )
import           GHC.Generics                   ( Generic )
import           Control.Lens
import           Control.Monad.Except
import           Data.Maybe



newtype ItemKey = ItemKey Int
    deriving (Show)
    deriving anyclass (FromJSON, ToJSON, ToSchema, HasFieldName)
newtype Quantity = Quantity Int
    deriving (Show)
    deriving anyclass (FromJSON, ToJSON, ToSchema, HasFieldName)
newtype ItemName = ItemName Text
    deriving (Show)
    deriving anyclass (FromJSON, ToJSON, ToSchema, HasFieldName)
newtype Price = Price Text
    deriving (Show)
    deriving anyclass (FromJSON, ToJSON, ToSchema, HasFieldName)

-- Command
data Store method a where
    BuyItem    ::ItemKey -> Quantity -> Store CMD ()
    ListItems :: Store QUERY (Map ItemKey ItemInfo)
    Admin :: Admin method a -> Store method a

data Admin method a where
    Restock    ::ItemKey -> Quantity -> Admin CMD ()
    AddItem    :: ItemName -> -> Admin CMD ItemInfo
    RemoveItem ::ItemKey -> Admin CMD ()

data StoreEvent
    = BoughtItem ItemKey Quantity
    | Restocked ItemKey Quantity
    | AddedItem ItemKey ItemInfo
    | RemovedItem ItemKey
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ItemInfo = ItemInfo
    { key :: ItemKey
    , name :: ItemName
    , quantity :: Quantity
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
        let iKey = succ <$> fromMaybe (Wrap 0) (maximumMay $ M.keys m)
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
