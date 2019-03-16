{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module DomainDriven.Internal.Class where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Kind
import           Data.UUID
import           RIO
import           RIO.Time
import           System.Random

data StmState x = StmState
    { writeEvent :: Stored (Event x) -> IO ()
    , readEvents :: IO [Event x]
    , currentState :: TVar x
    }

data a :|| b = a :|| b
    deriving (Show)
infixr 8 :||


class EventSourced model => IsCmd cmd model returns | cmd -> model, cmd -> returns where
    cmdHandler :: cmd -> ReaderT (StmState model) IO (Event model, returns)

class EventSourced model where
    type Event model :: Type

    applyEvent :: Stored (Event model) -> ReaderT (StmState model) IO ()

runCmd :: (IsCmd cmd model returns, EventSourced model)
       => cmd -> ReaderT (StmState model) IO returns
runCmd cmd = do
    (ev, r) <- cmdHandler cmd
    s <- toStored ev
    applyEvent s
    pure r

data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

mkId :: MonadIO m =>  (UUID -> b) -> m b
mkId c = c <$> liftIO randomIO

toStored :: MonadIO m => e -> m (Stored e)
toStored e = Stored e <$> getCurrentTime <*> mkId id
