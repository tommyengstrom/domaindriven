{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
module DomainDriven.Internal.Class where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Kind
import           Data.UUID
import           RIO
import           RIO.Time
import           System.Random


class EventSourced model where
    type Event model :: Type
    type Cmd model :: Type -> Type

    persistEvent :: Event model -> ReaderT model IO (Stored (Event model))
    applyEvent :: Stored (Event model) -> ReaderT model IO ()
    evalCmd :: Cmd model a -> ReaderT model IO (Event model, a)

    runCmd :: Cmd model a -> ReaderT model IO a
    runCmd cmd = do
        (ev, r) <- evalCmd cmd
        applyEvent =<< persistEvent ev
        pure r

data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

mkId :: MonadIO m =>  (UUID -> b) -> m b
mkId c = c <$> liftIO randomIO

-- Without a functional dep on EvenSourced (m -> model) model will have to be specified
-- when running.
toStored :: MonadIO m => e -> m (Stored e)
toStored e = Stored e <$> getCurrentTime <*> mkId id

data STMState x = STMState
    { writeEvent :: Stored (Event (STMState x)) -> IO ()
    , readEvents :: IO [Event (STMState x)]
    , currentState :: TVar x
    }
