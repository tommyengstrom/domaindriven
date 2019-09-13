import           RIO
import qualified RIO.List                                     as L
import           DomainDriven
import           Data.Aeson
import           Data.Generics.Product
import           GHC.TypeLits
import           GHC.Enum
import qualified Data.Map                                     as M
import           Test.Hspec
import           Safe                           ( headNote )
import           Control.Monad.Except

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

data ItemInfo = ItemInfo
    { quantity :: Quantity
    , price :: Int
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data StoreError
    = NotEnoughStock
    | NoSuchItem
    deriving (Show, Eq, Ord, Typeable)
instance Exception StoreError

type StoreModel = Map ItemKey ItemInfo

handleStoreCmd :: StoreCmd a -> IO (StoreModel -> Either StoreError (a, [StoreEvent]))
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



mkStoreModel :: IO (ESModel StoreModel StoreEvent StoreCmd StoreError IO)
mkStoreModel = ESModel noPersistance applyStoreEvent handleStoreCmd <$> newTVarIO mempty


-- | Number of unique products in stock
productCount :: StoreModel -> Int
productCount = length . M.keys . M.filter ((> 0) . view (typed @Quantity))

main :: IO ()
main = hspec . describe "Store model" $ do
    es <- runIO mkStoreModel
    it "Can add item" $ do
        let item :: ItemInfo
            item = ItemInfo 10 49
        iKey <- runCmd es $ AddItem item
        getModel es `shouldReturn` M.singleton iKey item

    it "Can buy item" $ do
        iKey <- headNote "Ops" . M.keys <$> getModel es
        runCmd es $ BuyItem iKey 7
        getModel es `shouldReturn` M.singleton (Wrap 1) (ItemInfo 3 49)


    it "Can run Query" $ do
        runQuery es productCount `shouldReturn` 1

        let item :: ItemInfo
            item = ItemInfo 4 33
        _ <- runCmd es $ AddItem item

        runQuery es productCount `shouldReturn` 2
