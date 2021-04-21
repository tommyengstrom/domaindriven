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

mapModel :: (m0 -> m1) -> HandlerType method m1 event a -> HandlerType method m0 event a
mapModel f = \case
    Query h -> Query (h . f)
    Cmd   h -> Cmd (h . f)

mapEvent :: (e0 -> e1) -> HandlerType method m e0 a -> HandlerType method m e1 a
mapEvent f = \case
    Query h -> Query h
    Cmd   h -> Cmd $ \m -> do
        (ret, evs) <- h m
        pure (ret, fmap f evs)

mapResult :: (r0 -> r1) -> HandlerType method m e r0 -> HandlerType method m e r1
mapResult f = \case
    Query h -> Query $ fmap f . h
    Cmd   h -> Cmd $ \m -> do
        (ret, evs) <- h m
        pure (f ret, evs)

class ReadModel p where
    type Model p :: Type
    type Event p :: Type
    applyEvent :: p -> Model p -> Stored (Event p) -> Model p
    getModel :: p -> IO (Model p)
    getEvents :: p -> IO [Stored (Event p)] -- TODO: Make it p stream!

class ReadModel p => WriteModel p where
    transactionalUpdate :: p -> IO (a, [Event p]) -> IO a


runAction
    :: (WriteModel p, model ~ Model p, event ~ Event p)
    => p
    -- -> (forall a . cmd method a -> HandlerType (CanMutate method) model event a)
    -> (forall a . cmd method a -> HandlerType method model event a)
    -> cmd method ret
    -> IO ret
runAction p handleCmd cmd = case handleCmd cmd of
    Query m -> m =<< getModel p
    Cmd   m -> transactionalUpdate p $ m =<< getModel p



class HasApiOptions (action :: (Type -> Type) -> Type -> Type) where
    apiOptions :: ApiOptions
    apiOptions = defaultApiOptions

data ApiOptions = ApiOptions
    { renameConstructor :: String -> [String]
    , typenameSeparator :: String
    , bodyNameBase      :: Maybe String
    }
    deriving Generic

defaultApiOptions :: ApiOptions
defaultApiOptions = ApiOptions { renameConstructor = pure
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
type ActionHandler model event cmd
    = forall method a . cmd method a -> HandlerType method model event a

type ActionRunner c = forall method a . c method a -> IO a

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
