{-# LANGUAGE TemplateHaskell #-}
    {-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains simple example of how to setup a counter using domain-driven.
module Main where

import           DomainDriven.Server
import           DomainDriven.Persistance.ForgetfulInMemory
import           DomainDriven
import           Prelude
import           Servant
import           Data.Typeable                  ( Typeable )
import           DomainDriven.Internal.Class
import qualified Data.ByteString.Lazy.Char8                   as BL
import           Servant.OpenApi
import           Control.Monad.Catch
import           Network.Wai.Handler.Warp       ( run )
import           Data.Aeson
import           GHC.Generics                   ( Generic )


-- | The model, representing the current state
type CounterModel = Int

data CounterEvent
    = CounterIncreased
    | CounterDecreased
    deriving (Show, Generic, ToJSON, FromJSON)

data CounterCmd method return where
   GetCounter ::CounterCmd QUERY Int
   IncreaseCounter ::CounterCmd CMD Int
   AddToCounter ::Int -> CounterCmd CMD Int

handleCmd
    :: CounterCmd method a -> ReturnValue (CanMutate method) CounterModel CounterEvent a
handleCmd = \case
    GetCounter      -> Query $ pure
    IncreaseCounter -> Cmd $ \m -> pure (m + 1, [CounterIncreased])
    AddToCounter a  -> Cmd $ \m -> pure (m + a, replicate a CounterIncreased)

data CounterError = NegativeNotSupported
    deriving (Show, Eq, Typeable, Exception)

applyCounterEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyCounterEvent m (Stored event _timestamp _uuid) = case event of
    CounterIncreased -> m + 1
    CounterDecreased -> m - 1

$(mkCmdServer defaultApiOptions ''CounterCmd)

main :: IO ()
--main = pure ()
main = do
    -- Pick a persistance model to create the domain model
    dm <- createForgetful applyCounterEvent 0
    BL.writeFile "/tmp/counter_schema.json" $ encode $ toOpenApi (Proxy @CounterCmdApi)
    -- Now we can supply the CmdRunner to the generated server and run it as any other
    -- Servant server.
    run 8888 $ serve (Proxy @CounterCmdApi) (counterCmdServer $ runCmd dm handleCmd)
