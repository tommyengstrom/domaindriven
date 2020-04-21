# Domain Driven

Domain driven is an eventsourcing/CQRS/DDD library. The goal of this library is to allow you to implement DDD principles without focusing on the boilerplate. Using `Template Haskell` we generate a Servant server from the model specification.

## The idea

- Focus on small scale systems that can run in memory (Models can be split up, e.g. per customer)
- Let the user focus on the actual problem by generating most of the code

## How it works

In order to implement a model in `domain-driven` you have to define:
- The model (current state)
- The events
- How to handle events
- The commands
- How to handle commands

### Model

The model is the current state of the system. This is what you normally would keep in a database.

Note that, since this library is based on eventsourcing, you often do not need to add in things you think you'll need down the line. Anything that is available in the events in some form can be included in the model at some later time.

### Events

Events are things that happened in the past. The event you define represent all the changes that can occur in the system.

### Event handler

The model is calculated as a fold over the stream of events. As events happened in the past we can never refuse to handle them. This means the event handler is simply:

``` haskell
applyEvent :: Model -> Stored Event -> Model
```

where Stored is defined as:
``` haskell
data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    }
```

### Commands

Commands are defined using a GADT with one type parameter represending the return type. For example:

``` haskell
data StorageCmd a where
    AddFile :: ByteString -> StorageCmd UUID
    RemoveFile :: UUID -> StorageCmd ()
```

### Command handler

Commands, in contrast to events, are allowed to fail. If a command succeeds we need to return a value of the type specified by the constructor and a list of events. The command handler do not update the state.

In addition you may need to make requests, read from disk, or perform other side effects in order to calculate the result.

`CmdHandler` is defined as:

``` haskell
type CmdHandler model event cmd err
    = forall a . Exception err => cmd a -> IO (model -> Either err (a, [event]))
```

Meaning when you get a command you can perform arbitrary side effects in IO, but in this stage you do not have access to the current model. Instead you have to return a continuation telling the library how to generate events or errors based on the model.

The reason for this approach is that it allows the library to guarantee that the model isn't updated where it should not be.


### Generating the server

Now we have defined the core parts of our domain. We can now generate the server using the template-haskell function `mkCmdServer`. It will generate a Servant API and server implementation. The only thing you need to do is to supply a `CmdRunner`.

To get a command runner we first have to define a persistance model, which tells us how the events are to be stored (note that the state is not persisted). We can then pick a start state and tie all the things together using `createModel`.

### Simple example

This is also available in [examples/simple/Main.hs]. For a slightly more realistic example check out [examples/hierarchical/Main.hs].


``` haskell
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


-- | The model, representing the current state
type CounterModel = Int

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
```
