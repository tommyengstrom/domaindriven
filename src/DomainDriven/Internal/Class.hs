module DomainDriven.Internal.Class where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Kind
import           Data.UUID
import           RIO
import           RIO.Time
import           System.Random


data STMState x = STMState
    { writeEvent :: Stored (Event x) -> IO ()
    , readEvents :: IO [Event x]
    , currentState :: TVar x
    }

class EventSourced a where
    type Event a :: Type
    type Cmd a :: Type -> Type

    applyEvent :: Stored (Event a) -> ReaderT (STMState a) IO ()
    evalCmd :: Cmd a r -> ReaderT (STMState a) IO (Event a, r)

    runCmd :: Cmd a r -> ReaderT (STMState a) IO r
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

