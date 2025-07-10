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

data CmdServer (model :: Type) (event :: Type) m a where
    Cmd :: HasCallStack => (model -> m (model -> a, [event])) -> CmdServer model event m a

data QueryServer (model :: Type) m a where
  Query :: HasCallStack => (model -> m a) -> QueryServer model m a

data CbQueryServer (model :: Type) m a where
  CbQuery :: ((forall n. (HasCallStack, MonadIO n) => n model) -> m a) -> CbQueryServer model m a

data CbCmdServer (model :: Type) (event :: Type) m a where
  CbCmd :: ((forall n b. (HasCallStack, MonadUnliftIO n) =>
        RunCmd model event n b) -> m a) -> CbCmdServer model event m a


instance MonadError ServerError m => ThrowAll (CmdServer model event m a) where
    throwAll = Cmd . throwAll

instance MonadError ServerError m => ThrowAll (CbCmdServer model event m a) where
    throwAll err = CbCmd $ \_ -> throwAll err

instance MonadError ServerError m => ThrowAll (QueryServer model m a) where
    throwAll = Query . throwAll

instance MonadError ServerError m => ThrowAll (CbQueryServer model m a) where
    throwAll err = CbQuery $ \_ -> throwAll err


data CmdServerI (index :: Type) (model :: Type) (event :: Type) m a
    = CmdI index (model -> m (model -> a, [event]))

data QueryServerI (index :: Type) (model :: Type) m a
    = QueryI index (model -> m a)

data CbQueryServerI (index :: Type) (model :: Type) m a
    = CbQueryI index ((forall n. MonadIO n => n model) -> m a)

data CbCmdServerI (index :: Type) (model :: Type) (event :: Type) m a
    = CbCmdI
        index
        ( ( forall n b
             . (HasCallStack, MonadUnliftIO n)
            => RunCmd model event n b
          )
          -> m a
        )

instance MonadError ServerError m => ThrowAll (CmdServerI model index event m a) where
    throwAll = CmdI undefined . throwAll

instance MonadError ServerError m => ThrowAll (CbCmdServerI model index event m a) where
    throwAll err = CbCmdI undefined $ \_ -> throwAll err

instance MonadError ServerError m => ThrowAll (QueryServerI model index m a) where
    throwAll = QueryI undefined . throwAll

instance MonadError ServerError m => ThrowAll (CbQueryServerI model index m a) where
    throwAll err = CbQueryI undefined $ \_ -> throwAll err

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

data WritePersistence model index event
    = forall p.
        ( Model p ~ model
        , Event p ~ event
        , Index p ~ index
        , WriteModel p
        ) =>
      WritePersistence p
data ReadPersistence model index
    = forall p.
        ( Model p ~ model
        , ReadModel p
        , Index p ~ index
        ) =>
      ReadPersistence p

instance
    ( HasServer (Verb method status ctypes a) context
    , CanMutate method ~ 'True
    , HasContextEntry context (WritePersistence model NoIndex event)
    )
    => HasServer (Cmd' model event (Verb method status ctypes a)) context
    where
    type
        ServerT (Cmd' model event (Verb method status ctypes a)) m =
            CmdServer model event m a
    hoistServerWithContext _ _ f (Cmd action) = Cmd $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: WritePersistence model NoIndex event of
            WritePersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(Cmd server) -> do
                            handlerRes <-
                                liftIO . Control.Monad.Catch.try . runCmd p NoIndex $
                                    either throwIO pure <=< runHandler . server
                            either throwError pure handlerRes
                        )
                        delayedServer
instance
    ( HasServer (Verb method status ctypes a) context
    , CanMutate method ~ 'True
    , HasContextEntry context (WritePersistence model index event)
    )
    => HasServer (CmdI' index model event (Verb method status ctypes a)) context
    where
    type
        ServerT (CmdI' index model event (Verb method status ctypes a)) m =
            CmdServerI index model event m a
    hoistServerWithContext _ _ f (CmdI index action) =
        CmdI index $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: WritePersistence model index event of
            WritePersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(CmdI index server) -> do
                            handlerRes <-
                                liftIO . Control.Monad.Catch.try . runCmd p index $
                                    either throwIO pure <=< runHandler . server
                            either throwError pure handlerRes
                        )
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (ReadPersistence model NoIndex)
    )
    => HasServer (Query' model (Verb method status ctypes a)) context
    where
    type ServerT (Query' model (Verb method status ctypes a)) m = QueryServer model m a

    hoistServerWithContext _ _ f (Query action) = Query $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: ReadPersistence model NoIndex of
            ReadPersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(Query server) -> server =<< liftIO (getModel p NoIndex)
                        )
                        delayedServer
instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (ReadPersistence model index)
    )
    => HasServer (QueryI' index model (Verb method status ctypes a)) context
    where
    type ServerT (QueryI' index model (Verb method status ctypes a)) m =
        QueryServerI index model m a

    hoistServerWithContext _ _ f (QueryI index action) =
        QueryI index $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: ReadPersistence model index of
            ReadPersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(QueryI index server) -> server =<< liftIO (getModel p index)
                        )
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (ReadPersistence model NoIndex)
    )
    => HasServer (CbQuery' model (Verb method status ctypes a)) context
    where
    type
        ServerT (CbQuery' model (Verb method status ctypes a)) m =
            CbQueryServer model m a

    hoistServerWithContext _ _ f (CbQuery action) = CbQuery $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: ReadPersistence model NoIndex of
            ReadPersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(CbQuery server) -> server (liftIO $ getModel p NoIndex)
                        )
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (ReadPersistence model index)
    )
    => HasServer (CbQueryI' index model (Verb method status ctypes a)) context
    where
    type
        ServerT (CbQueryI' index model (Verb method status ctypes a)) m =
            CbQueryServerI index model m a

    hoistServerWithContext _ _ f (CbQueryI index action) =
            CbQueryI index $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: ReadPersistence model index of
            ReadPersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(CbQueryI index server) -> server (liftIO $ getModel p index)
                        )
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , CanMutate method ~ 'True
    , HasContextEntry context (WritePersistence model NoIndex event)
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
        case getContextEntry context :: WritePersistence model NoIndex event of
            WritePersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(CbCmd server) -> server $ runCmd p NoIndex
                        )
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , CanMutate method ~ 'True
    , HasContextEntry context (WritePersistence model index event)
    )
    => HasServer (CbCmdI' index model event (Verb method status ctypes a)) context
    where
    type
        ServerT (CbCmdI' index model event (Verb method status ctypes a)) m =
            CbCmdServerI
                index
                model
                event
                m
                a

    hoistServerWithContext _ _ f (CbCmdI index action) =
        CbCmdI index $ \transact -> f (action transact)

    route _ context delayedServer =
        case getContextEntry context :: WritePersistence model index event of
            WritePersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(CbCmdI index server) -> server $ runCmd p index
                        )
                        delayedServer
