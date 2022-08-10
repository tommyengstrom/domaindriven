{-# LANGUAGE TemplateHaskell #-}
    {-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains simple example of how to setup a counter using domain-driven.
module Main where

import           Action
import           Control.Monad.Catch     hiding ( Handler )
import           Control.Monad.Except
import           Data.Aeson
import           Data.Bifunctor                 ( first )
import qualified Data.ByteString.Lazy.Char8                   as BL
import           DomainDriven
import           DomainDriven.Persistance.ForgetfulInMemory
import           DomainDriven.Server
import           Network.Wai.Handler.Warp       ( run )
import           Prelude
import           Servant
import           Servant.OpenApi

$(mkServer serverConfig ''CounterCmd)

main :: IO ()
main = do
    -- Pick a persistance model to create the domain model
    dm <- createForgetful applyCounterEvent 0
    BL.writeFile "/tmp/counter_schema.json" $ encode $ toOpenApi (Proxy @CounterCmdApi)
    -- Now we can supply the ActionRunner to the generated server and run it as any other
    -- Servant server.
    run 8888 $ serve (Proxy @CounterCmdApi) $ hoistServer
        (Proxy @CounterCmdApi)
        (Handler . ExceptT . fmap (first translateErrors) . try)
        (counterCmdServer $ runAction dm handleCmd)


-- | translate our custom errors into Servants server error. Any error that is not
-- translated will cause a 500 from the server.
translateErrors :: CounterError -> ServerError
translateErrors = \case
    NegativeNotSupported -> err422
        { errBody = "Operation is not allowed as it would make the counter negative"
        }
