{-# LANGUAGE TemplateHaskell #-}

-- | This module contains simple example of how to setup a counter using domain-driven.
module Main where

import           DomainDriven.Server            ( mkCmdServer )
import           DomainDriven
import           Prelude
import           Servant
import           Data.Typeable                  ( Typeable )
import           Control.Exception              ( Exception )
import           Network.Wai.Handler.Warp       ( run )


-- | The command GADT
-- The `a` represents the return value of the endpoints. If it is `()` domain-driven
-- will translate into `NoContent` (instead of using the servant encoding: `[]`)
data CounterCmd a where
   IncreaseCounter ::CounterCmd ()
   DecreaseCounter ::CounterCmd ()

-- | The events that commands can generate
data CounterEvent
    = CounterIncreased
    | CounterDecreased
    deriving (Show)

type CounterModel = Int

data CounterError
    = NegativeNotSupported
    deriving (Show, Eq, Typeable, Exception)

applyCounterEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyCounterEvent m (Stored event _timestamp _uuid) = case event of
    CounterIncreased -> m + 1
    CounterDecreased -> m - 1

-- CmdHandler resolves to:
--      `forall a . Exception err => cmd a -> IO (model -> Either err (a, [event]))`
-- Meaning we first have access to IO to perform arbitrary actions, then we return a
-- plan for how to handle it when we see the model.
-- In this case we do not need to use the IO so we return right away.
handleCounterCmd :: CmdHandler CounterModel CounterEvent CounterCmd CounterError
handleCounterCmd = \case
    IncreaseCounter -> do
        putStrLn "About to increase counter!" -- Full IO access before seeing the model
        pure $ \_ -> Right ((), [CounterIncreased])
    DecreaseCounter -> do
        putStrLn "Someone asked for a decrease..."
        pure
            $ \m -> if m > 0
                  then Right ((), [CounterDecreased])
                  else Left NegativeNotSupported

-- | mkCmdServer will generate a servant API and server for the command GADT provided
-- The API type will be named `CounterCmdApi` and the server will be named
-- `counterCmdServer`.
$(mkCmdServer ''CounterCmd)

-- | Start a server running on port 8765
-- Try it out with:
-- `curl localhost:8765/IncreaseCounter -X POST`
-- `curl localhost:8765/DecreaseCounter -X POST`
main :: IO ()
main = do
    -- First we need to pick a persistance model.
    persistanceModel <- noPersistance
    -- Then we need to create the model
    model            <- createModel persistanceModel applyCounterEvent 0
    -- Now we can supply the CmdRunner to the generated server and run it as any other
    -- Servant server.
    run 8765 $ serve (Proxy @CounterCmdApi)
                     (counterCmdServer $ runCmd model handleCounterCmd)
