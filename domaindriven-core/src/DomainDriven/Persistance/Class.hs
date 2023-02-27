{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Persistance.Class where

import Control.DeepSeq (NFData)
import Control.Monad.Reader
import Data.Aeson
import Data.Kind
import Data.Time
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Streamly.Prelude (SerialT)
import System.Random
import UnliftIO
import Prelude

-- class ReadModel p where
--     type Model p :: Type
--     type Event p :: Type
--     applyEvent :: p -> Model p -> Stored (Event p) -> Model p
--     getModel :: p -> IO (Model p)
--     getEventList :: p -> IO [Stored (Event p)]
--     getEventStream :: p -> SerialT IO (Stored (Event p))

data AllIndices = AllIndices
    deriving (Show, Eq)

class ReadModel p where
    type Model p :: Type
    type Event p :: Type
    type Index p :: Type
    applyEvent
        :: Index p
        -> p
        -> Model p
        -> Stored (Event p)
        -> Model p
    getModel
        :: Index p
        -> p
        -> IO (Model p)
    getEventList
        :: Index p
        -> p
        -> IO [Stored (Event p)]
    getEventStream
        :: Index p
        -> p
        -> SerialT IO (Stored (Event p))

class ReadModel p => WriteModel p where
    transactionalUpdate
        :: forall m a
         . MonadUnliftIO m
        => Index p
        -> p
        -> (Model p -> m (Model p -> a, [Event p]))
        -> m a

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

mkId :: MonadIO m => m UUID
mkId = liftIO randomIO

toStored :: MonadIO m => e -> m (Stored e)
toStored e = Stored e <$> liftIO getCurrentTime <*> mkId
