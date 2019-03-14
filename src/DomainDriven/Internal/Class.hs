{-# LANGUAGE AllowAmbiguousTypes #-}
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

class EventSourced a where
    type Event a :: Type
    type Cmd a :: Type -> Type

    applyEvent :: Stored (Event a) -> ReaderT (StmState a) IO ()
    evalCmd :: Cmd a r -> ReaderT (StmState a) IO (Event a, r)

    runCmd :: Cmd a r -> ReaderT (StmState a) IO r
    runCmd cmd = do
        (ev, r) <- evalCmd cmd
        f <- asks writeEvent
        s <- toStored ev
        liftIO $ f s
        applyEvent s
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


-- I think this approach may actually work out okay. Though I'm not quite sure about
-- how I could generate a server from this. I guess I need template haskell anyway.
--
-- The upside is that now each command can be a record and I have the structures already,
-- so the only thing I need template haskell for is to fetch the class instances (I think)
class HasCmd model cmd where
    type Event' cmd :: Type
    type Return' cmd :: Type
    applyEvent' :: Stored (Event' cmd) -> ReaderT (StmState model) IO ()
    evalCmd' :: cmd -> ReaderT (StmState model) IO (Event' cmd, Return' cmd)

