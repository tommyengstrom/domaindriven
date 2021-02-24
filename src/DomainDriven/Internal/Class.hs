{-# LANGUAGE UndecidableInstances #-}
    {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module DomainDriven.Internal.Class where

import           Control.Monad.Reader
import           Data.Aeson
import           Prelude
import           Data.Time
import           System.Random
import           Control.Monad.Catch
import           GHC.Generics                   ( Generic )
import           Data.Void
import           Servant
import           GHC.TypeLits
import           Data.Kind
import           Data.UUID                      ( UUID )

--data AfterUpdate a
--    HandlerReturn (HandlerType 'GET code cts) model event (AfterUpdate a) =
--        TypeError ('Text "GET methods cannot update the model")
--    HandlerReturn (HandlerType 'POST code cts) model event (AfterUpdate a) =
--        (model -> a, [event])

--class Monad m => HasModel m model where
--    fetchModel :: m model

-- | This is `Verb` from servant, but without the return type
-- data HandlerType (method :: StdMethod) (statusCode :: Nat) (contentTypes :: [Type])

type CMD = Verb 'POST 200 '[JSON]
type QUERY = Verb 'GET 200 '[JSON]

-- Instead of StdMethod I could use something that carries more information, namely
-- content-type and return code. I could then define type aliases `Cmd` and `Query`
type family HandlerReturn model event method a where
    HandlerReturn model event (Verb 'GET code cts)  a = ReturnValue 'False model event a
    HandlerReturn model event (Verb 'POST code cts) a = ReturnValue 'True model event a
    HandlerReturn model event (Verb 'PUT code cts) a = ReturnValue 'True model event a
    HandlerReturn model event (Verb 'PATCH code cts) a = ReturnValue 'True model event a
    HandlerReturn model event (Verb 'DELETE code cts) a = ReturnValue 'True model event a

-- | This duplicates HandlerReturn. I wasn't able to get GHC to understand the types with
type family CanMutate method :: Bool where
    CanMutate (Verb 'GET code cts) = 'False
    CanMutate (Verb 'POST code cts) = 'True
    CanMutate (Verb 'PUT code cts) = 'True
    CanMutate (Verb 'PATCH code cts) = 'True
    CanMutate (Verb 'DELETE code cts) = 'True

data ReturnValue mutates model event a where
    Query ::(model -> IO a) -> ReturnValue 'False model event a
    Cmd ::(model -> IO (a, [event])) -> ReturnValue 'True model event a
    -- Query :: (model -> IO a) -> ReturnValue model Void err a
    -- Cmd   :: IO (model -> (a, [event])) -> ReturnValue model event err a


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
    -> (forall a . cmd method a -> ReturnValue (CanMutate method) model event a)
    -> cmd method ret
    -> IO ret
runCmd p handleCmd cmd = case handleCmd cmd of
    Query m -> m =<< getModel p
    Cmd   m -> transactionalUpdate p $ m =<< getModel p
--------------------------------------------
--class Monad m => DDView m model where
--    fetchModel :: m model
--
--class DDView m model => DDWrite m model event where
--    transUpdate :: m (a, [event]) -> m a
--
----newtype DD p a = DD (ReaderT p IO a)
----    deriving newtype (Functor, Applicative, Monad, MonadIO)
--
--instance (ReadModel p, model ~ Model p)=> DDView (ReaderT p IO) model where
--    fetchModel =  do
--        p <- ask
--        lift $ getModel p


--class ProcessReturnValue p event where
--    doit :: p -> ReturnValue (Model p) event err a -> IO a
--
--instance ReadModel p => ProcessReturnValue p Void where
--    doit p = \case
--        Query f -> do
--           m <- getModel p
--           f m
--           m <- getModel p
--           let (a, evs) = cont m -- This can never happen as evs would be [Void]
--           _ <- traverse absurd evs
--           pure a

-- instance WriteModel p => ProcessReturnValue p (Event p) where
--     doit p = \case
--         Query f -> do
--            m <- getModel p
--            f m
--         Cmd f -> do
--            cont <- f
--            m <- getModel p
--            let (_, v) = cont m
--            absurd v

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
--type QueryHandler model query err
--    = forall a . Exception err => model -> query a -> IO (Either err a)

type CmdRunner c = forall method a . c method a -> IO a

--runCmd
--    :: forall err m verb a cmd . (WriteModel m)
--    => m
--    -> (cmd verb a -> HandlerReturn (Model m) (Event m) err verb a)
--    -> cmd verb a
--    -> IO a
--runCmd m cmdRunner cmd = do
--    cmdRunner cmd >>= transactionalUpdate @_ @_ @err m



-- | Run a query
-- runQuery :: (Exception err, ReadModel rm)
--     => rm
--     -> (Model rm -> query a -> IO (Either err a))
--     -> query a
--     -> IO a
-- runQuery rm queryRunner query = do
--     m <- getModel rm
--     r <- queryRunner m query
--     either throwM pure r

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
