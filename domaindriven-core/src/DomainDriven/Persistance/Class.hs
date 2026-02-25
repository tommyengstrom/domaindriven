module DomainDriven.Persistance.Class where

import Control.DeepSeq (NFData)
import Control.Monad.Reader
import Data.Aeson
import Data.Hashable (Hashable)
import Data.Kind
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import GHC.Generics (Generic)
import GHC.Stack
import Streamly.Data.Stream.Prelude (Stream)
import System.IO (hPutStrLn)
import System.Random
import UnliftIO
import Prelude

data NoIndex = NoIndex
    deriving (Show, Eq, Ord, Generic, Hashable)

newtype Indexed = Indexed Text
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Hashable)

class ReadModel p where
    type Model p :: Type
    type Event p :: Type
    type Index p :: Type
    applyEvent :: p -> Model p -> Stored (Event p) -> Model p
    getModel :: MonadIO m => HasCallStack => p -> Index p -> m (Model p)
    getEventList :: p -> Index p -> IO [Stored (Event p)]
    getEventStream :: HasCallStack => p -> Index p -> Stream IO (Stored (Event p))

class ReadModel p => WriteModel p where
    -- | Hook to call after model has been updated.
    -- This allows for setting up outgoing hooks in calling out to external systems.
    -- This is run asynchronously after update is processed.
    postUpdateHook
        :: MonadIO m
        => p
        -> Index p
        -> Model p
        -> [Stored (Event p)]
        -> m ()

    transactionalUpdate
        :: HasCallStack
        => forall m a
         . MonadUnliftIO m
        => p
        -> Index p
        -> (Model p -> m (Model p -> a, [Event p]))
        -> m
            ( Model p
            , [Stored (Event p)]
            , (Model p -> a)
            )
        -- ^ How to create the return value from updated model

-- | Wrapper for stored data
-- This ensures all events have a unique ID and a timestamp, without having to deal with
-- that when implementing the model.
data Stored a = Stored
    { storedEvent :: a
    , storedTimestamp :: UTCTime
    , storedUUID :: UUID
    }
    deriving
        ( Show
        , Eq
        , Ord
        , Generic
        , FromJSON
        , ToJSON
        , Functor
        , Foldable
        , Traversable
        , NFData
        )

runCmd
    :: HasCallStack
    => forall p m a
     . (WriteModel p, MonadUnliftIO m)
    => p
    -> Index p
    -> (Model p -> m (Model p -> a, [Event p]))
    -> m a
runCmd p index cmd = withFrozenCallStack $ do
    (model, events, returnFun) <- transactionalUpdate p index cmd
    _ <- liftIO $ async $ postUpdateHook p index model events `catchAny` \e ->
        hPutStrLn stderr $ "[DomainDriven] postUpdateHook failed: " <> displayException e
    pure $ returnFun model

mkId :: MonadIO m => m UUID
mkId = liftIO randomIO

toStored :: MonadIO m => e -> m (Stored e)
toStored e = Stored e <$> liftIO getCurrentTime <*> mkId
