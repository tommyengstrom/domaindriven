{-# LANGUAGE ImplicitParams #-}
-- needed for context entry. todo: non-type-driven context lookup!
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DomainDriven.Server.Server where

import Control.Monad
import Control.Monad.Catch qualified
import Control.Monad.Except
import Data.Kind
import DomainDriven.Persistance.Class
import DomainDriven.Server.Api
import GHC.Stack
import GHC.TypeLits
import Servant hiding (inject)
import Servant.Auth.Server
import Servant.Server.Internal.Delayed
import UnliftIO hiding (Handler)
import Prelude

data CmdServer (model :: Type) (event :: Type) m a
    = Cmd CallStack (model -> m (model -> a, [event]))

mkCmd :: HasCallStack => (model -> m (model -> a, [event])) -> CmdServer model event m a
mkCmd = Cmd callStack

data QueryServer (model :: Type) m a = Query CallStack (model -> m a)

mkQuery :: HasCallStack => (model -> m a) -> QueryServer model m a
mkQuery = Query callStack

newtype CbQueryServer (model :: Type) m a
    = CbQuery ((forall n. (HasCallStack, MonadIO n) => n model) -> m a)

mkCbQuery
    :: ((forall n. (HasCallStack, MonadIO n) => n model) -> m a) -> CbQueryServer model m a
mkCbQuery = CbQuery

newtype CbCmdServer (model :: Type) (event :: Type) m a
    = CbCmd ((forall n b. (HasCallStack, MonadUnliftIO n) => RunCmd model event n b) -> m a)

mkCbCmdServer
    :: ((forall n b. (HasCallStack, MonadUnliftIO n) => RunCmd model event n b) -> m a)
    -> CbCmdServer model event m a
mkCbCmdServer = CbCmd

instance MonadError ServerError m => ThrowAll (CmdServer model event m a) where
    throwAll = Cmd callStack . throwAll

instance MonadError ServerError m => ThrowAll (CbCmdServer model event m a) where
    throwAll err = CbCmd $ \_ -> throwAll err

instance MonadError ServerError m => ThrowAll (QueryServer model m a) where
    throwAll = Query callStack . throwAll

instance MonadError ServerError m => ThrowAll (CbQueryServer model m a) where
    throwAll err = CbQuery $ \_ -> throwAll err

type family CanMutate (method :: StdMethod) :: Bool where
    CanMutate 'GET = 'False
    CanMutate 'POST = 'True
    CanMutate 'PUT = 'True
    CanMutate 'PATCH = 'True
    CanMutate 'DELETE = 'True
    CanMutate method =
        TypeError
            ( 'Text "CanMutate is not defined for "
                ':<>: 'ShowType method
            )

mapServer :: (a -> b) -> Delayed env a -> Delayed env b
mapServer f Delayed{..} =
    Delayed
        { serverD = \c p h a b req -> fmap f (serverD c p h a b req)
        , ..
        }

data WritePersistence model event
    = forall p. (Model p ~ model, Event p ~ event, WriteModel p) => WritePersistence p
data ReadPersistence model = forall p. (Model p ~ model, ReadModel p) => ReadPersistence p

instance
    ( HasServer (Verb method status ctypes a) context
    , CanMutate method ~ 'True
    , HasContextEntry context (WritePersistence model event)
    )
    => HasServer (Cmd' model event (Verb method status ctypes a)) context
    where
    type
        ServerT (Cmd' model event (Verb method status ctypes a)) m =
            CmdServer model event m a
    hoistServerWithContext _ _ f (Cmd theCallStack action) = Cmd theCallStack $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: WritePersistence model event of
            WritePersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(Cmd theCallStack server) -> do
                            let ?callStack = theCallStack
                            handlerRes <-
                                liftIO . Control.Monad.Catch.try . runCmd p $
                                    either throwIO pure <=< runHandler . server
                            either throwError pure handlerRes
                        )
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (ReadPersistence model)
    )
    => HasServer (Query' model (Verb method status ctypes a)) context
    where
    type ServerT (Query' model (Verb method status ctypes a)) m = QueryServer model m a

    hoistServerWithContext _ _ f (Query theCallStack action) = Query theCallStack $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: ReadPersistence model of
            ReadPersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(Query theCallStack server) ->
                            let ?callStack = theCallStack
                             in server =<< liftIO (getModel p)
                        )
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (ReadPersistence model)
    )
    => HasServer (CbQuery' model (Verb method status ctypes a)) context
    where
    type
        ServerT (CbQuery' model (Verb method status ctypes a)) m =
            CbQueryServer model m a

    hoistServerWithContext _ _ f (CbQuery action) = CbQuery $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: ReadPersistence model of
            ReadPersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(CbQuery server) -> server (liftIO $ getModel p)
                        )
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , CanMutate method ~ 'True
    , HasContextEntry context (WritePersistence model event)
    )
    => HasServer (CbCmd' model event (Verb method status ctypes a)) context
    where
    type
        ServerT (CbCmd' model event (Verb method status ctypes a)) m =
            CbCmdServer
                model
                event
                m
                a

    hoistServerWithContext _ _ f (CbCmd action) =
        CbCmd $ \transact -> f (action transact)

    route _ context delayedServer =
        case getContextEntry context :: WritePersistence model event of
            WritePersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(CbCmd server) -> server $ runCmd p
                        )
                        delayedServer
