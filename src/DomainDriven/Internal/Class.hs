{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module DomainDriven.Internal.Class where

import           Control.Monad.Reader
import           Data.Aeson
import           Prelude
import           Data.Time
import           System.Random
import           Control.Monad.Catch
import           GHC.Generics                   ( Generic )
import           Servant
import           Data.Kind
import           Data.UUID                      ( UUID )

type CMD = Verb 'POST 200 '[JSON]
type QUERY = Verb 'GET 200 '[JSON]

-- | This duplicates HandlerReturn. I wasn't able to get GHC to understand the types with
type family CanMutate (method :: Type -> Type) :: Bool where
    CanMutate (Verb 'GET code cts) = 'False
    CanMutate (Verb 'POST code cts) = 'True
    CanMutate (Verb 'PUT code cts) = 'True
    CanMutate (Verb 'PATCH code cts) = 'True
    CanMutate (Verb 'DELETE code cts) = 'True

data HandlerType (method :: Type -> Type) model event a where
    Query ::CanMutate method ~ 'False => (model -> IO a) -> HandlerType method model event a
    Cmd ::CanMutate method ~ 'True => (model -> IO (a, [event])) -> HandlerType method model event a


class ReadModel p where
    type Model p :: Type
    type Event p :: Type
    applyEvent :: p -> Model p -> Stored (Event p) -> Model p
    getModel :: p -> IO (Model p)
    getEvents :: p -> IO [Stored (Event p)] -- TODO: Make it p stream!

class ReadModel p => WriteModel p where
    transactionalUpdate :: p -> IO (a, [Event p]) -> IO a


runCmd
    :: (WriteModel p, model ~ Model p, event ~ Event p)
    => p
    -- -> (forall a . cmd method a -> HandlerType (CanMutate method) model event a)
    -> (forall a . cmd method a -> HandlerType method model event a)
    -> cmd method ret
    -> IO ret
runCmd p handleCmd cmd = case handleCmd cmd of
    Query m -> m =<< getModel p
    Cmd   m -> transactionalUpdate p $ m =<< getModel p

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

type CmdRunner c = forall method a . c method a -> IO a

-- | Wrapper for stored data
-- This ensures all events have a unique ID and a timestamp, without having to deal with
-- that when implementing the model.
data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    }
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, Functor)

mkId :: MonadIO m => m UUID
mkId = liftIO randomIO

toStored :: MonadIO m => e -> m (Stored e)
toStored e = Stored e <$> liftIO getCurrentTime <*> mkId
