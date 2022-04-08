{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module DomainDriven.Internal.Class where

import           Control.Monad.Reader
import           Data.Aeson
import           Prelude
import           Control.Lens                   ( (^.) )
import           Data.Generics.Product
import           Data.Time
import           System.Random
import           Control.DeepSeq (NFData)
import           GHC.Generics                   ( Generic )
import           Servant
import           Data.Kind
import           Data.UUID                      ( UUID )
import UnliftIO
import Streamly.Prelude (SerialT)

data RequestType (contentTypes :: [Type]) (verb :: Type -> Type)
type Cmd = RequestType '[JSON] (Verb 'POST 200 '[JSON])
type Query = RequestType '[JSON] (Verb 'GET 200 '[JSON])

type family CanMutate method :: Bool where
    CanMutate (RequestType c (Verb 'GET code cts)) = 'False
    CanMutate (RequestType c (Verb 'POST code cts)) = 'True
    CanMutate (RequestType c (Verb 'PUT code cts)) = 'True
    CanMutate (RequestType c (Verb 'PATCH code cts)) = 'True
    CanMutate (RequestType c (Verb 'DELETE code cts)) = 'True


data HandlerType method model event m a where
    Query :: CanMutate method ~ 'False
          => (model -> m a)
          -> HandlerType method model event m a
    Cmd :: CanMutate method ~ 'True
        => (model -> m (model -> a, [event]))
        -> HandlerType method model event m a

mapModel
    :: Monad m
    => (model0 -> model1)
    -> HandlerType method model1 event m a
    -> HandlerType method model0 event m a
mapModel f = \case
    Query h -> Query (h . f)
    Cmd   h -> Cmd $ \m -> do
        (fm, evs) <- h $ f m
        pure (fm . f, evs)

mapEvent
    :: Monad m
    => (e0 -> e1)
    -> HandlerType method model e0 m a
    -> HandlerType method model e1 m a
mapEvent f = \case
    Query h -> Query h
    Cmd   h -> Cmd $ \m -> do
        (ret, evs) <- h m
        pure (ret, fmap f evs)

mapResult
    :: Monad m
    => (r0 -> r1)
    -> HandlerType method model e m r0
    -> HandlerType method model e m r1
mapResult f = \case
    Query h -> Query $ fmap f . h
    Cmd   h -> Cmd $ \m -> do
        (ret, evs) <- h m
        pure (f . ret, evs)

class ReadModel p where
    type Model p :: Type
    type Event p :: Type
    applyEvent :: p -> Model p -> Stored (Event p) -> Model p
    getModel :: p -> IO (Model p)
    getEventList :: p -> IO [Stored (Event p)] -- TODO: Make it p stream!
    getEventStream :: p -> SerialT IO (Stored (Event p))

class ReadModel p  => WriteModel p where
    transactionalUpdate :: forall m a. MonadUnliftIO m
                        => p
                        -> (Model p -> m (Model p -> a, [Event p]))
                        -> m a


runAction
    :: (MonadUnliftIO m, WriteModel p, model ~ Model p, event ~ Event p)
    => p
    -> (forall a . cmd method a -> HandlerType method model event m a)
    -> cmd method ret
    -> m ret
runAction p handleCmd cmd = case handleCmd cmd of
    Query m -> m =<< liftIO (getModel p)
    Cmd   m -> transactionalUpdate p m



class HasApiOptions (action :: Type -> Type -> Type) where
    apiOptions :: ApiOptions
    apiOptions = defaultApiOptions

data ApiOptions = ApiOptions
    { renameConstructor :: String -> [String]
    , renamePathParams :: String -> String
    , typenameSeparator :: String
    , bodyNameBase      :: Maybe String
    }
    deriving Generic

defaultApiOptions :: ApiOptions
defaultApiOptions = ApiOptions { renameConstructor = pure
                               , renamePathParams = id
                               , typenameSeparator = "_"
                               , bodyNameBase      = Nothing
                               }

instance Show ApiOptions  where
    show o =
        "ApiOptions {renameConstructor = ***, typenameSeparator = \""
            <> o
            ^. field @"typenameSeparator"
            <> "\"}"


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
type ActionHandler model event m cmd
    = forall method a . cmd method a -> HandlerType method model event m a

type ActionRunner m c = forall method a . c method a -> m a

-- | Wrapper for stored data
-- This ensures all events have a unique ID and a timestamp, without having to deal with
-- that when implementing the model.
data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, Functor, Foldable, Traversable
              , NFData)

mkId :: MonadIO m => m UUID
mkId = liftIO randomIO

toStored :: MonadIO m => e -> m (Stored e)
toStored e = Stored e <$> liftIO getCurrentTime <*> mkId
