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
import           Data.Kind
import           GHC.IO.Unsafe                  ( unsafePerformIO )

class EventSourced model where
    type Event model :: Type
    applyEvent :: MonadIO m => TVar model -> Stored (Event model) -> m ()
    readEvents :: MonadIO m => m [Stored(Event x)]
    persistEvent :: MonadIO m => Event x -> m (Stored (Event x))


------------------------------------------------------------
------------- Idea of what it should look like -------------
------------------------------------------------------------
data EventError
    = EventError404 Text
    | EventError401
    deriving (Show, Eq, Ord, Typeable)
instance Exception EventError

--class EventSourced' model where
--    type Event' model :: Type
--    applyEvent' :: model -> Stored (Event' model) -> model

type family EvType model :: Type

data ESModel model = ESModel
    { persistance :: Persistance model
    , appEvent :: model -> Stored (EvType model) -> model
    , model :: TVar model
    }

data ESView model = ESView
    { evantChan :: TChan (Stored (EvType model)) -- Would be nice to express that things
                                                 -- can not be put in
    , appEvent :: model -> Stored (EvType model) -> model
    , model :: TVar model
    }

-- These methods should be replaced with streams down the line
data Persistance model = Persistance
    { readEvents' :: IO [Stored(EvType model)]
    -- , persistEvent' :: EvType model -> IO (Stored(EvType model))
    , persistEvent' :: Stored (EvType model) -> IO ()
    }

filePersistance :: (ToJSON e, FromJSON e) => FilePath -> Persistance e
filePersistance fp =
    Persistance { readEvents' = undefined fp, persistEvent' = undefined fp }

noPersistance :: Persistance e
noPersistance = Persistance { readEvents' = pure [], persistEvent' = const $ pure () }

-- Things to handle/answer
-- [*] Persistance
-- [*] Accessing the model
-- [*] Broadcasting events to other views (only one write model)
-- [ ] How are commands specified?
--
-- Command, what I want
-- * One ADT
-- * ADT specifies return value
-- * Ability to generate a servant API and server from the ADT

data MyEvent =
    MyEv1 | MyEv2 Int | MyEv3
    deriving (Show, Eq, Ord, Generic)

data MyCmd return where
    AddUser ::Text -> MyCmd Int
    DelUser ::Int -> MyCmd ()

runMyCmd :: MyCmd r -> IO ([MyEvent], r)
runMyCmd = \case
    AddUser _ -> pure ([], 8)
    DelUser _ -> pure ([], ())

data MyQuery return where
    GetUser ::Int -> MyQuery [Text]

runMyQuery :: MyQuery r -> IO r
runMyQuery = \case
    GetUser _ -> pure ["Hulk Hogan"]



runCmd
    :: (MonadIO m, MonadThrow m, Exception err)
    => ESModel model
    -> (cmd a -> m (model -> Either err (a, [EvType model])))
    -> cmd a
    -> m a
runCmd (ESModel pm appE tvar) runner cmd = do
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


------------------------------------------------------------
-----------------------End of idea--------------------------
------------------------------------------------------------

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


runCmd'
    :: (EventSourced model, Command cmd model, MonadIO m, MonadThrow m)
    => (cmd -> m (CmdDeps cmd))
    -> TVar model
    -> cmd
    -> m (CmdReturn cmd)
runCmd' deps tvar cmd = do
    (ev, r) <- cmdHandler deps tvar cmd
    s       <- toStored ev
    applyEvent tvar s
    pure r

data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

mkId :: MonadIO m => (UUID -> b) -> m b
mkId c = c <$> liftIO randomIO

toStored :: MonadIO m => e -> m (Stored e)
toStored e = Stored e <$> getCurrentTime <*> mkId id
