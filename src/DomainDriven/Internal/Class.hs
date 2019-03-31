{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module DomainDriven.Internal.Class where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.UUID
import           RIO
import           RIO.Time
import           System.Random
import Data.Kind

class EventSourced model where
    type Event model :: Type
    applyEvent :: MonadIO m => TVar model -> Stored (Event model) -> m ()
    readEvents :: MonadIO m => m [Event x]
    persistEvent :: MonadIO m => Stored (Event x) -> m ()

class Query query where
    type QueryDeps query :: Type
    type QueryReturn query :: Type

    runQuery :: (MonadThrow m, MonadIO m)
             => (query -> m (QueryDeps query)) -> query -> m (QueryReturn query)

class EventSourced model => Command cmd model | cmd -> model where
    type CmdDeps cmd :: Type
    type CmdReturn cmd :: Type
    cmdHandler :: (MonadThrow m, MonadIO m)
             => (cmd -> m (CmdDeps cmd))
             -> TVar model
             -> cmd
             -> m (Event model, CmdReturn cmd)


runCmd :: (EventSourced model, Command cmd model, MonadIO m, MonadThrow m)
       => (cmd -> m (CmdDeps cmd))
       -> TVar model
       -> cmd
       -> m (CmdReturn cmd)
runCmd deps tvar cmd = do
    (ev, r) <- cmdHandler deps tvar cmd
    s <- toStored ev
    applyEvent tvar s
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
