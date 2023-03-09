{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Server.Class where

import Control.Monad.Reader
import Data.Kind
import DomainDriven.Persistance.Class
import GHC.TypeLits
import Servant
import UnliftIO
import Prelude

data
    RequestType
        (accessType :: ModelAccess)
        (contentTypes :: [Type])
        (verb :: Type -> Type)

data ModelAccess
    = Direct
    | Callback

type Cmd = RequestType 'Direct '[JSON] (Verb 'POST 200 '[JSON])
type CbCmd = RequestType 'Callback '[JSON] (Verb 'POST 200 '[JSON])
type Query = RequestType 'Direct '[JSON] (Verb 'GET 200 '[JSON])
type CbQuery = RequestType 'Callback '[JSON] (Verb 'GET 200 '[JSON])

-- | The kind of an Action, defined with a GADT as:
-- data MyAction :: Action where
--    ThisAction :: P x "count" Int -> MyAction x 'Cmd Int
--    ThatAction :: P x "description" Text -> MyAction x 'Cmd ()
type Action = ParamPart -> Type -> Type -> Type

type family CanMutate method :: Bool where
    CanMutate (RequestType a c (Verb 'GET code cts)) = 'False
    CanMutate (RequestType a c (Verb 'POST code cts)) = 'True
    CanMutate (RequestType a c (Verb 'PUT code cts)) = 'True
    CanMutate (RequestType a c (Verb 'PATCH code cts)) = 'True
    CanMutate (RequestType a c (Verb 'DELETE code cts)) = 'True

-- | Used as a parameter to the `P` type family on order to determine the focus.
data ParamPart
    = ParamName
    | ParamType
    deriving (Show)

-- | P is used for specifying the parameters of the model.
-- The name will be used as the name in the JSON encoding or the query parameter of the
-- generated server.
type family P (x :: ParamPart) (name :: Symbol) (a :: Type) where
    P 'ParamName name ty = Proxy name
    P 'ParamType name ty = ty

type family GetModelAccess method :: ModelAccess where
    GetModelAccess (RequestType a b c) = a

data HandlerType method model event m a where
    Query
        :: (CanMutate method ~ 'False, GetModelAccess method ~ 'Direct)
        => (model -> m a)
        -> HandlerType method model event m a
    CbQuery
        :: (CanMutate method ~ 'False, GetModelAccess method ~ 'Callback)
        => (m model -> m a)
        -> HandlerType method model event m a
    Cmd
        :: (CanMutate method ~ 'True, GetModelAccess method ~ 'Direct)
        => (model -> m (model -> a, [event]))
        -> HandlerType method model event m a
    CbCmd
        :: (CanMutate method ~ 'True, GetModelAccess method ~ 'Callback)
        => ((forall x. (model -> m (model -> x, [event])) -> m x) -> m a)
        -> HandlerType method model event m a

type CmdCallback model event (m :: Type -> Type) =
    (forall a. model -> m (a, [event]))

mapModel
    :: forall m event model0 model1 method a
     . Monad m
    => (model0 -> model1)
    -> HandlerType method model1 event m a
    -> HandlerType method model0 event m a
mapModel f = \case
    Query h -> Query (h . f)
    CbQuery withModel -> CbQuery \fetchModel ->
        withModel (fmap f fetchModel)
    Cmd h -> Cmd $ \m -> do
        (fm, evs) <- h $ f m
        pure (fm . f, evs)
    CbCmd withTrans -> CbCmd $ \runTrans ->
        withTrans $ \(trans :: model -> m (x, [e0])) -> do
            runTrans $ \model -> do
                (r, evs) <- trans (f model)
                pure (r . f, evs)

mapEvent
    :: forall m e0 e1 a method model
     . Monad m
    => (e0 -> e1)
    -> HandlerType method model e0 m a
    -> HandlerType method model e1 m a
mapEvent f = \case
    Query h -> Query h
    CbQuery h -> CbQuery h
    Cmd h -> Cmd $ \m -> do
        (ret, evs) <- h m
        pure (ret, fmap f evs)
    CbCmd withTrans -> CbCmd $ \runTrans ->
        withTrans $ \(trans :: model -> m (x, [e0])) -> do
            runTrans $ \model -> do
                (r, evs) <- trans model
                pure (r, fmap f evs)

mapResult
    :: Monad m
    => (r0 -> r1)
    -> HandlerType method model e m r0
    -> HandlerType method model e m r1
mapResult f = \case
    Query h -> Query $ fmap f . h
    CbQuery h -> CbQuery $ fmap f . h
    Cmd h -> Cmd $ \m -> do
        (ret, evs) <- h m
        pure (f . ret, evs)
    CbCmd withTrans -> CbCmd $ \transact -> f <$> withTrans transact

-- | Action handler
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
type ActionHandler model event m c =
    forall method a. c 'ParamType method a -> HandlerType method model event m a

type ActionRunner m c =
    forall method a
     . MonadUnliftIO m
    => c 'ParamType method a
    -> m a

runAction
    :: (MonadUnliftIO m, WriteModel p, model ~ Model p, event ~ Event p)
    => p
    -> ActionHandler model event m cmd
    -> cmd 'ParamType method ret
    -> m ret
runAction p handleCmd cmd = case handleCmd cmd of
    Query m -> m =<< liftIO (getModel p)
    CbQuery m -> m (liftIO (getModel p))
    Cmd m -> transactionalUpdate p m
    CbCmd withTrans -> withTrans $ \runTrans -> do
        transactionalUpdate p runTrans
