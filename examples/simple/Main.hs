{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains simple example of how to setup a counter using domaindriven.
module Main where

import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Data.Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as BL
import DomainDriven
import DomainDriven.Persistance.ForgetfulInMemory
import DomainDriven.Server
import Models.Counter
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.OpenApi
import Prelude

$(mkServer serverConfig ''CounterAction)

main :: IO ()
main = do
    -- Pick a persistance model to create the domain model
    dm <- createForgetful applyCounterEvent 0
    BL.writeFile "/tmp/counter_schema.json" $ encode $ toOpenApi (Proxy @CounterActionApi)
    -- Now we can supply the ActionRunner to the generated server and run it as any other
    -- Servant server.
    run 8888 $
        serve (Proxy @CounterActionApi) $
            hoistServer
                (Proxy @CounterActionApi)
                (Handler . ExceptT . fmap (first translateErrors) . try)
                (counterActionServer $ runAction dm handleAction)

-- | translate our custom errors into Servants server error. Any error that is not
-- translated will cause a 500 from the server.
translateErrors :: CounterError -> ServerError
translateErrors = \case
    NegativeNotSupported ->
        err422
            { errBody = "Operation is not allowed as it would make the counter negative"
            }
