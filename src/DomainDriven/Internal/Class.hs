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
import           GHC.IO.Unsafe                  ( unsafePerformIO )


data ESModel model event cmd err m = ESModel
    { persistance :: Persistance event
    , applyEvent :: model -> Stored event -> model
    , cmdHandler :: forall a . (Exception err, MonadIO m, MonadThrow m)
                 => cmd a -> m (model -> Either err (a, [event]))
    , esModel :: TVar model
    } -- deriving Generic

data ESView model event = ESView
    { evantChan :: TChan (Stored event)
    , applyEvent :: model -> Stored event -> model
    , esvModel :: TVar model
    } deriving Generic

-- These methods should be replaced with streams down the line
data Persistance event = Persistance
    { readEvents' :: IO [Stored event]
    , persistEvent' :: Stored event -> IO ()
    } deriving Generic

filePersistance :: (ToJSON e, FromJSON e) => FilePath -> Persistance e
filePersistance fp =
    Persistance { readEvents' = undefined fp, persistEvent' = undefined fp }

noPersistance :: Persistance e
noPersistance = Persistance { readEvents' = pure [], persistEvent' = const $ pure () }

runCmd
    :: (Exception err, MonadIO m, MonadThrow m)
    => ESModel model event cmd err m
    -> cmd a
    -> m a
runCmd (ESModel pm appE runner tvar) cmd = do
    runnerTrans <- runner cmd
    (r, evs)    <- atomically $ do
        m        <- readTVar tvar
        (r, evs) <- either throwM pure $ runnerTrans m
        let storedEvs = fmap (unsafePerformIO . toStored) evs
            newModel  = foldl' appE m storedEvs
        writeTVar tvar newModel
        pure (r, storedEvs)
    traverse_ (liftIO . persistEvent' pm) evs
    pure r

class HasModel es model | es -> model where
    getModel :: MonadIO m => es -> m model

instance HasModel (ESModel model e c err m) model where
    getModel = liftIO . readTVarIO . esModel

instance HasModel (ESView model event) model where
    getModel = liftIO . readTVarIO . esvModel

-- | runQuery is reall just readTVar and apply the function...
-- But we will likely want to not export the TVar containing the model, in order to
-- enfore the library is being used correctly.
runQuery :: (HasModel es model, MonadIO m) => es -> (model -> a) -> m a
runQuery es f = f <$> getModel es

data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

mkId :: MonadIO m => (UUID -> b) -> m b
mkId c = c <$> liftIO randomIO

toStored :: MonadIO m => e -> m (Stored e)
toStored e = Stored e <$> getCurrentTime <*> mkId id
