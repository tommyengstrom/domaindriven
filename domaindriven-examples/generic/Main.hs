{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains simple example of how to setup a counter using domaindriven.
module Main where

import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Counter
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import DomainDriven
import DomainDriven.Persistance.ForgetfulInMemory
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.OpenApi
import Prelude

writeSchema :: IO ()
writeSchema =
    BL.writeFile "/tmp/store_schema.json" $ encode $ toOpenApi (Proxy @StoreActionApi)

main :: IO ()
main = do
    -- Pick a persistance model to create the domain model
    m <- createForgetful applyStoreEvent mempty
    writeSchema
    -- Now we can supply the ActionRunner to the generated server and run it as any other
    -- Servant server.
    run 8888 $
        serve (Proxy @StoreActionApi) $
            hoistServer
                (Proxy @StoreActionApi)
                (Handler . ExceptT . try)
                (storeActionServer $ runAction m handleStoreAction)
