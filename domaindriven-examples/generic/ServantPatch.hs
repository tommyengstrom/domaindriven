{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServantPatch where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Kind
import Data.Typeable (Typeable)
import DomainDriven
import GHC.Generics (Generic)
import Servant
import Servant.Server.Generic
import Servant.Server.Internal.Delayed
import Prelude

data Cmd' (p :: Type) (verb :: Type)

mapServer :: (a -> b) -> Delayed env a -> Delayed env b
mapServer f Delayed{..} =
    Delayed
        { serverD = \c p h a b req -> fmap f (serverD c p h a b req)
        , ..
        }

instance
    ( WriteModel p
    , HasContextEntry context p
    , HasServer (Verb method status ctypes a) context
    )
    => HasServer (Cmd' p (Verb method status ctypes a)) context
    where
    type ServerT (Cmd' p (Verb method status ctypes a)) m = Model p -> m (Model p -> a, [Event p])
    hoistServerWithContext _ _ f action model = f (action model)
    route _ context delayedServer =
        route
            (Proxy @(Verb method status ctypes a))
            context
            ( mapServer
                ( \server -> do
                    handlerRes :: Either ServerError a <-
                        liftIO
                            . Control.Monad.Catch.try
                            . transactionalUpdate (getContextEntry context :: p)
                            $ either throwM pure <=< runHandler . server
                    either throwError pure handlerRes
                )
                delayedServer
            )
