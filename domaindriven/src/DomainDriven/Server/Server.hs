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

data CmdServer (model :: Type) (index :: Type) (event :: Type) m a
    = Cmd index (model -> m (model -> a, [event]))

data QueryServer (model :: Type) (index :: Type) m a
    = Query index (model -> m a)

data CbQueryServer (model :: Type) (index :: Type) m a
    = CbQuery index ((forall n. MonadIO n => n model) -> m a)

data CbCmdServer (model :: Type) (index :: Type) (event :: Type) m a
    = CbCmd
        index
        ( ( forall n b
             . MonadUnliftIO n
            => TransactionalUpdate model event n b
          )
          -> m a
        )

instance MonadError ServerError m => ThrowAll (CmdServer index model event m a) where
    throwAll = Cmd undefined . throwAll

instance MonadError ServerError m => ThrowAll (CbCmdServer model index event m a) where
    throwAll err = CbCmd undefined $ \_ -> throwAll err

instance MonadError ServerError m => ThrowAll (QueryServer model index m a) where
    throwAll = Query undefined . throwAll

instance MonadError ServerError m => ThrowAll (CbQueryServer model index m a) where
    throwAll err = CbQuery undefined $ \_ -> throwAll err

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
    , HasContextEntry context (WritePersistence model index event)
    )
    => HasServer (Cmd' model index event (Verb method status ctypes a)) context
    where
    type
        ServerT (Cmd' model index event (Verb method status ctypes a)) m =
            CmdServer model index event m a
    hoistServerWithContext _ _ f (Cmd i action) = Cmd i $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: WritePersistence model index event of
            WritePersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        ( \(Cmd index server) -> do
                            handlerRes <-
                                liftIO
                                    . Control.Monad.Catch.try
                                    . transactionalUpdate p index
                                    $ either throwIO pure <=< runHandler . server
                            either throwError pure handlerRes
                        )
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (ReadPersistence model index)
    )
    => HasServer (Query' model index (Verb method status ctypes a)) context
    where
    type
        ServerT (Query' model index (Verb method status ctypes a)) m =
            QueryServer model index m a

    hoistServerWithContext _ _ f (Query index action) =
        Query index $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: ReadPersistence model index of
            ReadPersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        (\(Query index server) -> server =<< liftIO (getModel p index))
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , HasContextEntry context (ReadPersistence model index)
    )
    => HasServer (CbQuery' model index (Verb method status ctypes a)) context
    where
    type
        ServerT (CbQuery' model index (Verb method status ctypes a)) m =
            CbQueryServer model index m a

    hoistServerWithContext _ _ f (CbQuery i action) = CbQuery i $ \model -> f (action model)

    route _ context delayedServer =
        case getContextEntry context :: ReadPersistence model index of
            ReadPersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        (\(CbQuery index server) -> server $ liftIO $ getModel p index)
                        delayedServer

instance
    ( HasServer (Verb method status ctypes a) context
    , CanMutate method ~ 'True
    , HasContextEntry context (WritePersistence model index event)
    )
    => HasServer (CbCmd' model index event (Verb method status ctypes a)) context
    where
    type
        ServerT (CbCmd' model index event (Verb method status ctypes a)) m =
            CbCmdServer model index event m a

    hoistServerWithContext _ _ f (CbCmd index action) =
        CbCmd index $ \transact -> f (action transact)

    route _ context delayedServer =
        case getContextEntry context :: WritePersistence model index event of
            WritePersistence p ->
                route (Proxy @(Verb method status ctypes a)) context $
                    mapServer
                        (\(CbCmd index server) -> server $ transactionalUpdate p index)
                        delayedServer
