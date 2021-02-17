{-# LANGUAGE AllowAmbiguousTypes #-}
module DomainDriven.Internal.Class where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.UUID
import           Data.Kind
import           Prelude
import           Data.Time
import           System.Random
import           Control.Monad.Catch
import           GHC.Generics                   ( Generic )
import           Servant
import           GHC.TypeLits

type family Mutating (handler :: Type -> Type -> Type) :: Bool where
    Mutating (h 'GET a) = 'False
    Mutating (h 'POST a) = 'True

data AfterUpdate a

class Monad m => HasModel m model where
    readModel :: m model

-- | This is `Verb` from servant, but without the return type
data HandlerType (method :: StdMethod) (statusCode :: Nat) (contentTypes :: [Type])

type Cmd = HandlerType 'POST 200 '[JSON]
type Query = HandlerType 'GET 200 '[JSON]

-- Instead of StdMethod I could use something that carries more information, namely
-- content-type and return code. I could then define type aliases `Cmd` and `Query`
type family HandlerReturn verb event model a where
    HandlerReturn (HandlerType 'GET code cts) event model (AfterUpdate a) =
        TypeError ('Text "GET methods cannot update the model")
    HandlerReturn (HandlerType 'GET code cts) event model a = a
    HandlerReturn (HandlerType 'POST code cts) event model (AfterUpdate a) =
        (model -> a, [event])
    HandlerReturn (HandlerType 'POST code cts) event model a = (a, [event])


class ReadModel a where
    type Model a :: Type
    type Event a :: Type
    applyEvent :: a -> Model a -> Stored (Event a) -> Model a
    getModel :: a -> IO (Model a)
    getEvents :: a -> IO [Stored (Event a)] -- TODO: Make it a stream!


class ReadModel a => WriteModel a where
    transactionalUpdate :: forall ret err.
        Exception err =>
           a
          -> (Model a -> Either err (ret, [Event a]))
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
type QueryHandler model query err
    = forall a . Exception err => model -> query a -> IO (Either err a)

type CmdRunner c = forall a . c a -> IO a
type QueryRunner c = forall a . c a -> IO a

runCmd
    :: (Exception err, WriteModel m)
    => m
    -> CmdHandler (Model m) (Event m) cmd err
    -> cmd a
    -> IO a
runCmd m cmdRunner cmd = do

    cmdRunner cmd >>= transactionalUpdate m



-- | Run a query
runQuery
    :: (Exception err, ReadModel rm)
    => rm
    -> (Model rm -> query a -> IO (Either err a))
    -> query a
    -> IO a
runQuery rm queryRunner query = do
    m <- getModel rm
    r <- queryRunner m query
    either throwM pure r

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
