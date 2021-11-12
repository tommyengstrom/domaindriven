{-# LANGUAGE TemplateHaskell #-}
 {-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains simple example of how to setup a counter using domain-driven.
module Main where

import           App
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8                   as BL
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           DomainDriven
import           DomainDriven.Persistance.ForgetfulInMemory
import           DomainDriven.Server
import           Language.Haskell.TH
import           Network.Wai.Handler.Warp       ( run )
import           Prelude
import           Servant
import           Servant.OpenApi

$(mkServer serverConfig ''CounterCmd)

main :: IO ()
main = do
    -- Pick a persistance model to create the domain model
    dm <- createForgetful applyCounterEvent 0
    let apiDefPath = "/tmp/counter_schema.json"
    putStrLn $ "Writing api definition to: " <> show apiDefPath
    BL.writeFile apiDefPath $ encode $ toOpenApi (Proxy @CounterCmdApi)
    -- Now we can supply the ActionRunner to the generated server and run it as any other
    -- Servant server.
    run 8888 $ serve (Proxy @CounterCmdApi) (counterCmdServer $ runAction dm handleCmd)


$(mkRecord "Olle" (ConstructorArg (mkName "apa") (ConT ''Int) :| []))
