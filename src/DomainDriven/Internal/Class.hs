{-# LANGUAGE AllowAmbiguousTypes #-}
module DomainDriven.Internal.Class where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.UUID
import           Data.Kind
import           RIO
import           RIO.Time
import           System.Random

data DomainModel persist model event = DomainModel
    { persistanceHandler :: persist
        -- ^ An implementation of `PersistanceHandler`.
    , applyEvent         :: model -> Stored event -> model
        -- ^ How to calculate the next state
    }

class ReadModel a where
    type Model a :: Type
    type Event a :: Type
    applyEvent' :: a -> Model a -> Stored (Event a) -> Model a
    getModel' :: a -> IO (Model a)
    getEvents' :: a -> IO [Stored (Event a)] -- TODO: Make it a stream!


class ReadModel a => WriteModel a where
    type Error a :: Type
    transactionalUpdate' :: forall ret
                          . a
                          -> (Model a -> Either (Error a) (ret, [Event a]))
                         -> IO ret

-- | Command handler
--
-- Expects a command, specified using a one-parameter GADT where the parameter specifies
-- the return type.
--
-- When implementing the handler you have access to IO, but in order for the library to
-- ensure thread safety of state updates you do not have direct access to the current
-- state. Instead the handler returns a continuation, telling the library how to perform
-- the evaluations on the model.
--
-- The resulting events will be applied to the current state so that no other command can
-- run and generate events on the same state.
type CmdHandler model event cmd err
    = forall a . Exception err => cmd a -> IO (model -> Either err (a, [event]))

type CmdRunner c = forall a . c a -> IO a
type QueryRunner c = forall a . c a -> IO a

class PersistanceHandler a model event | a -> model, a -> event where
    getModel :: a -> IO model
    getEvents :: a -> IO [event] -- ^ FIXME: This should really be a stream of some sort!
    -- | How to perform updates
    -- It is important that the state cannot be changed be updated between aquiring the
    -- current state and writing the events.
    transactionalUpdate
        :: Exception err
        => a
        -> (model -> Stored event -> model)
            -- ^ The apply function of the model
        -> (model -> Either err (ret, [event]))
            -- ^ The continuation returned by CmdHandler
        -> IO ret
--runCmd'
--    :: WriteModel model event err
--    => DomainModel persist model event
--    -> CmdHandler model event cmd err
--    -> cmd a
--    -> IO a
--runCmd' (DomainModel pm appEvent) cmdRunner cmd =
--    cmdRunner cmd >>= transactionalUpdate pm appEvent


runCmd
    :: (Exception err, PersistanceHandler persist model event)
    => DomainModel persist model event
    -> CmdHandler model event cmd err
    -> cmd a
    -> IO a
runCmd (DomainModel pm appEvent) cmdRunner cmd =
    cmdRunner cmd >>= transactionalUpdate pm appEvent

-- | Run a query
runQuery
    :: (Exception err, PersistanceHandler persist model event)
    => DomainModel persist model event
    -> (model -> query a -> IO (Either err a))
    -> query a
    -> IO a
runQuery (DomainModel pm _) f query = do
    m <- getModel pm
    r <- f m query
    either throwM pure r

-- | Wrapper for stored data
-- This ensures all events have a unique ID and a timestamp, without having to deal with
-- that when implementing the model.
data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

mkId :: MonadIO m => m UUID
mkId = liftIO randomIO

toStored :: MonadIO m => e -> m (Stored e)
toStored e = Stored e <$> getCurrentTime <*> mkId
