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

class ReadModel model where
    type Event model :: Type
    applyEvent :: MonadIO m => TVar model -> Stored (Event model) -> m ()
    readEvents :: MonadIO m => m [Event x]

class ReadModel model => Query query model ret | query -> model, query -> ret where
    runQuery :: (MonadThrow m, MonadIO m) => TVar model -> query -> m ret

class ReadModel model => DomainModel model where
    persistEvent :: MonadIO m => Stored (Event x) -> m ()

class DomainModel model => Command cmd model ret | cmd -> model, cmd -> ret where
    cmdHandler :: (MonadThrow m, MonadIO m) => TVar model -> cmd -> m (Event model, ret)

runCmd :: (Command cmd model ret, DomainModel model, MonadIO m, MonadThrow m)
       => TVar model -> cmd -> m ret
runCmd tvar cmd = do
    (ev, r) <- cmdHandler tvar cmd
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
