{-# LANGUAGE TemplateHaskell #-}
    {-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains simple example of how to setup a counter using domain-driven.
module Main where

import           Action
import           Control.Monad.Catch     hiding ( Handler )
import           Control.Monad.Except
import           Data.Aeson
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
        (Handler . ExceptT . try)
        (counterCmdServer $ runAction dm handleCmd)
