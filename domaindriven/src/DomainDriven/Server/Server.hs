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
import GHC.TypeLits
import Servant hiding (inject)
import Servant.Auth.Server
import Servant.Server.Internal.Delayed
import UnliftIO hiding (Handler)
import Prelude

data CmdServer (model :: Type) (event :: Type) m a
    = Cmd (model -> m (model -> a, [event]))

data QueryServer (model :: Type) m a
    = Query (model -> m a)

data CbQueryServer (model :: Type) m a
    = CbQuery ((forall n. MonadIO n => n model) -> m a)

data CbCmdServer (model :: Type) (event :: Type) m a
    = CbCmd
        ( ( forall n b
             . MonadUnliftIO n
            => TransactionalUpdate model event n b
          )
          -> m a
        )

data CmdServerI (model :: Type) (index :: Type) (event :: Type) m a
    = CmdI index (model -> m (model -> a, [event]))

data QueryServerI (model :: Type) (index :: Type) m a
    = QueryI index (model -> m a)

data CbQueryServerI (model :: Type) (index :: Type) m a
    = CbQueryI index ((forall n. MonadIO n => n model) -> m a)

data CbCmdServerI (model :: Type) (index :: Type) (event :: Type) m a
    = CbCmdI
        index
        ( ( forall n b
             . MonadUnliftIO n
            => TransactionalUpdate model event n b
          )
          -> m a
        )

instance MonadError ServerError m => ThrowAll (CmdServerI index model event m a) where
    throwAll = CmdI undefined . throwAll

instance MonadError ServerError m => ThrowAll (CmdServer model event m a) where
    throwAll = Cmd . throwAll

instance MonadError ServerError m => ThrowAll (CbCmdServer model event m a) where
    throwAll err = CbCmd $ \_ -> throwAll err

instance MonadError ServerError m => ThrowAll (CbCmdServerI model index event m a) where
    throwAll err = CbCmdI undefined $ \_ -> throwAll err

instance MonadError ServerError m => ThrowAll (QueryServer model m a) where
    throwAll = Query . throwAll

instance MonadError ServerError m => ThrowAll (QueryServerI model index m a) where
    throwAll = QueryI undefined . throwAll

instance MonadError ServerError m => ThrowAll (CbQueryServer model m a) where
    throwAll err = CbQuery $ \_ -> throwAll err

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
    {-# OVERLAPPABLE #-}
    ( HasServer (Verb method status ctypes a) context
    , CanMutate method ~ 'True
    , HasContextEntry context (WritePersistence model index event)
    )
    => HasServer (CmdI' model index event (Verb method status ctypes a)) context
    where
    type
        ServerT (CmdI' model index event (Verb method status ctypes a)) m =
            CmdServerI model index event m a
    hoistServerWithContext _ _ f (CmdI i action) = CmdI i $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: WritePersistence model index event of
            WritePersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(CmdI index server) -> do
                            handlerRes <-
                                liftIO
                                    . Control.Monad.Catch.try
                                    . transactionalUpdate p index
                                    $ either throwIO pure <=< runHandler . server
                            either throwError pure handlerRes
                        )
                        delayedServer

instance
    {-# OVERLAPPABLE #-}
    ( HasServer (Verb method status ctypes a) context
    , CanMutate method ~ 'True
    , HasContextEntry context (WritePersistence model () event)
    )
    => HasServer (Cmd' model event (Verb method status ctypes a)) context
    where
    type
        ServerT (Cmd' model event (Verb method status ctypes a)) m =
            CmdServer model event m a
    hoistServerWithContext _ _ f (Cmd action) = Cmd $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: WritePersistence model () event of
            WritePersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(Cmd server) -> do
                            handlerRes <-
                                liftIO
                                    . Control.Monad.Catch.try
                                    . transactionalUpdate p ()
                                    $ either throwIO pure <=< runHandler . server
                            either throwError pure handlerRes
                        )
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (ReadPersistence model index)
    )
    => HasServer (QueryI' model index (Verb method status ctypes a)) context
    where
    type
        ServerT (QueryI' model index (Verb method status ctypes a)) m =
            QueryServerI model index m a

    hoistServerWithContext _ _ f (QueryI index action) =
        QueryI index $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: ReadPersistence model index of
            ReadPersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        (\(QueryI index server) -> server =<< liftIO (getModel p index))
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (ReadPersistence model ())
    )
    => HasServer (Query' model (Verb method status ctypes a)) context
    where
    type
        ServerT (Query' model (Verb method status ctypes a)) m =
            QueryServer model m a

    hoistServerWithContext _ _ f (Query action) =
        Query $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: ReadPersistence model () of
            ReadPersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        (\(Query server) -> server =<< liftIO (getModel p ()))
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (ReadPersistence model index)
    )
    => HasServer (CbQueryI' model index (Verb method status ctypes a)) context
    where
    type
        ServerT (CbQueryI' model index (Verb method status ctypes a)) m =
            CbQueryServerI model index m a

    hoistServerWithContext _ _ f (CbQueryI i action) = CbQueryI i $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: ReadPersistence model index of
            ReadPersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        (\(CbQueryI index server) -> server $ liftIO $ getModel p index)
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (ReadPersistence model ())
    )
    => HasServer (CbQuery' model (Verb method status ctypes a)) context
    where
    type
        ServerT (CbQuery' model (Verb method status ctypes a)) m =
            CbQueryServer model m a

    hoistServerWithContext _ _ f (CbQuery action) = CbQuery $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: ReadPersistence model () of
            ReadPersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        (\(CbQuery server) -> server $ liftIO $ getModel p ())
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , CanMutate method ~ 'True
    , HasContextEntry context (WritePersistence model index event)
    )
    => HasServer (CbCmdI' model index event (Verb method status ctypes a)) context
    where
    type
        ServerT (CbCmdI' model index event (Verb method status ctypes a)) m =
            CbCmdServerI model index event m a

    hoistServerWithContext _ _ f (CbCmdI index action) =
        CbCmdI index $ \transact -> f (action transact)

    route _ context delayedServer =
        case getContextEntry context :: WritePersistence model index event of
            WritePersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        (\(CbCmdI index server) -> server $ transactionalUpdate p index)
                        delayedServer
instance
    ( HasServer (Verb method status ctypes a) context
    , CanMutate method ~ 'True
    , HasContextEntry context (WritePersistence model () event)
    )
    => HasServer (CbCmd' model event (Verb method status ctypes a)) context
    where
    type
        ServerT (CbCmd' model event (Verb method status ctypes a)) m =
            CbCmdServer model event m a

    hoistServerWithContext _ _ f (CbCmd action) =
        CbCmd $ \transact -> f (action transact)

    route _ context delayedServer =
        case getContextEntry context :: WritePersistence model () event of
            WritePersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        (\(CbCmd server) -> server $ transactionalUpdate p ())
                        delayedServer
