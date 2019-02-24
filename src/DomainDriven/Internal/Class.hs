{-# LANGUAGE FunctionalDependencies #-}
module DomainDriven.Internal.Class where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.UUID
import           RIO
import           RIO.Time
import           System.Random

data DDException
    = StorageError Text
    deriving (Show)

instance Exception DDException

class DomainModel model cmd event | model -> cmd, model -> event where
    initial :: model
    applyEvent :: model -> Stored event -> model
    evalCmd :: model -> cmd -> IO [event]

data EventStore event = EventStore
    { readEvents :: IO [Stored event]
    , storeEvent :: event -> IO (Stored event)
    } deriving Generic

data DModel model cmd event = DModel
    { initial'    :: model
    , applyEvent' :: model -> Stored event -> model
    , evalCmd'    :: model -> cmd -> IO [event]
    } deriving Generic

data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, Functor)

data STMState model cmd event = STMState
    { store :: EventStore event
    , model :: DModel model cmd event
    , state :: MVar model
    } deriving Generic

class DomainDriven m where
    type Model m :: *
    type Event m :: *
    type Cmd m :: *
    loadEvents :: m ()
    getProjection :: m (Model m)
    runCmd :: Cmd m -> m [Stored (Event m)]


mkId :: MonadIO m =>  (UUID -> b) -> m b
mkId c = c <$> liftIO randomIO
