-- needed for context entry. todo: non-type-driven context lookup!
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DomainDriven.Server.Servant.Server where

import Control.Monad
import Control.Monad.Catch qualified
import Control.Monad.Except
import Data.Kind
import DomainDriven.Persistance.Class
import DomainDriven.Server.Servant.Api
import GHC.TypeLits
import Servant hiding (inject)
import Servant.Auth.Server
import Servant.Server.Internal.Delayed
import UnliftIO hiding (Handler)
import Prelude

newtype CmdServer (model :: Type) (event :: Type) m a
    = Cmd (model -> m (model -> a, [event]))

newtype QueryServer (model :: Type) m a = Query (model -> m a)

newtype CbQueryServer (model :: Type) m a
    = CbQuery ((forall n. MonadIO n => n model) -> m a)

newtype CbCmdServer (model :: Type) (event :: Type) m a
    = CbCmd ((forall n b. MonadUnliftIO n => TransactionalUpdate model event n b) -> m a)

instance (MonadError ServerError m) => ThrowAll (CmdServer model event m a) where
    throwAll = Cmd . throwAll

instance (MonadError ServerError m) => ThrowAll (CbCmdServer model event m a) where
    throwAll err = CbCmd $ \_ -> throwAll err

instance (MonadError ServerError m) => ThrowAll (QueryServer model m a) where
    throwAll = Query . throwAll

instance (MonadError ServerError m) => ThrowAll (CbQueryServer model m a) where
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

newtype Persistence p = Persistence p

type family LookupPersistance (context :: [Type]) :: Type where
    LookupPersistance (Persistence p ': _) = p
    LookupPersistance (_ ': context) = LookupPersistance context
    LookupPersistance '[] = TypeError ( 'Text "Context does not contain a Persistence!")

instance
    ( HasServer (Verb method status ctypes a) context
    , CanMutate method ~ 'True
    , HasContextEntry context (Persistence (LookupPersistance context))
    , WriteModel (LookupPersistance context)
    , model ~ Model (LookupPersistance context)
    , event ~ Event (LookupPersistance context)
    )
    => HasServer (Cmd model event (Verb method status ctypes a)) context
    where
    type
        ServerT (Cmd model event (Verb method status ctypes a)) m =
            CmdServer model event m a
    hoistServerWithContext _ _ f (Cmd action) = Cmd $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: Persistence (LookupPersistance context) of
            Persistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(Cmd server) -> do
                            handlerRes <-
                                liftIO . Control.Monad.Catch.try . transactionalUpdate p $
                                    either throwIO pure <=< runHandler . server
                            either throwError pure handlerRes
                        )
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (Persistence (LookupPersistance context))
    , ReadModel (LookupPersistance context)
    , model ~ Model (LookupPersistance context)
    )
    => HasServer (Query model (Verb method status ctypes a)) context
    where
    type ServerT (Query model (Verb method status ctypes a)) m = QueryServer model m a

    hoistServerWithContext _ _ f (Query action) = Query $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: Persistence (LookupPersistance context) of
            Persistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        (\(Query server) -> server =<< liftIO (getModel p))
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (Persistence (LookupPersistance context))
    , ReadModel (LookupPersistance context)
    , model ~ Model (LookupPersistance context)
    )
    => HasServer (CbQuery model (Verb method status ctypes a)) context
    where
    type
        ServerT (CbQuery model (Verb method status ctypes a)) m =
            CbQueryServer model m a

    hoistServerWithContext _ _ f (CbQuery action) = CbQuery $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: Persistence (LookupPersistance context) of
            Persistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        (\(CbQuery server) -> server $ liftIO $ getModel p)
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , CanMutate method ~ 'True
    , HasContextEntry context (Persistence (LookupPersistance context))
    , WriteModel (LookupPersistance context)
    , model ~ Model (LookupPersistance context)
    , event ~ Event (LookupPersistance context)
    )
    => HasServer (CbCmd model event (Verb method status ctypes a)) context
    where
    type
        ServerT (CbCmd model event (Verb method status ctypes a)) m =
            CbCmdServer
                model
                event
                m
                a

    hoistServerWithContext _ _ f (CbCmd action) =
        CbCmd $ \transact -> f (action transact)

    route _ context delayedServer =
        case getContextEntry context :: Persistence (LookupPersistance context) of
            Persistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        (\(CbCmd server) -> server $ transactionalUpdate p)
                        delayedServer
