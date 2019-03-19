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
    applyEvent :: Stored (Event model) -> ReaderT (TVar model) IO ()
    readEvents :: IO [Event x]

class ReadModel model => Query query model ret | query -> model, query -> ret where
    runQuery :: query -> ReaderT (TVar model) IO ret

class ReadModel model => DomainModel model where
    persistEvent :: Stored (Event x) -> IO ()

class DomainModel model => Command cmd model ret | cmd -> model, cmd -> ret where
    cmdHandler :: cmd -> ReaderT (TVar model) IO (Event model, ret)
    -- runCommand :: cmd -> ReaderT (TVar model) IO (Event model)
    -- returnValue :: Event model -> ret

runCmd :: (Command cmd model ret, DomainModel model)
       => cmd -> ReaderT (TVar model) IO ret
runCmd cmd = do
    (ev, r) <- cmdHandler cmd
    -- ev <- runCommand cmd
    s <- toStored ev
    applyEvent s
    pure r
    -- pure $ returnValue s

data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

mkId :: MonadIO m =>  (UUID -> b) -> m b
mkId c = c <$> liftIO randomIO

toStored :: MonadIO m => e -> m (Stored e)
toStored e = Stored e <$> getCurrentTime <*> mkId id
