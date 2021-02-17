{-# LANGUAGE TemplateHaskell #-}

-- | This module contains simple example of how to setup a counter using domain-driven.
module Main where

import           DomainDriven.Server
import           DomainDriven.Persistance.ForgetfulSTM
import           DomainDriven
import           Prelude
import           Servant
import           Data.Typeable                  ( Typeable )
import           DomainDriven.Internal.Class
import           Control.Exception              ( Exception )
import           Control.Monad.Catch
import           Control.Monad
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
   IncreaseCounter ::CounterCmd Cmd (AfterUpdate Int)
   AddToCounter ::Int -> CounterCmd Cmd Int
   GetCounter ::CounterCmd Query Int

handleCmd
    :: (HasModel m CounterModel, MonadThrow m)
    => CounterCmd method a
    -> m (HandlerReturn method CounterEvent CounterModel a)
handleCmd = \case
    GetCounter      -> readModel
    IncreaseCounter -> pure (id, [CounterIncreased])
    AddToCounter a  -> do
        i <- readModel
        pure (i + a, [])


data CounterError = NegativeNotSupported
    deriving (Show, Eq, Typeable, Exception)

applyCounterEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyCounterEvent m (Stored event _timestamp _uuid) = case event of
    CounterIncreased -> m + 1
    CounterDecreased -> m - 1

$(mkCmdServer defaultApiOptions ''CounterCmd)

-- | Start a server running on port 8765
-- Try it out with:
-- `curl localhost:8765/IncreaseCounter -X POST`
-- -- `curl localhost:8765/DecreaseCounter -X POST`
main :: IO ()
main = pure ()
-- main = do
--     -- Pick a persistance model to create the domain model
--     dm <- createForgetfulSTM applyCounterEvent 0
--     -- Now we can supply the CmdRunner to the generated server and run it as any other
--     -- Servant server.
--     run 8765
--         $ serve (Proxy @CounterCmdApi) (counterCmdServer $ runCmd dm handleCounterCmd)
