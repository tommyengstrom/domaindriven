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

`ActionHandler` is defined as:

``` haskell
type ActionHandler model event cmd err
    = forall a . Exception err => cmd a -> IO (model -> Either err (a, [event]))
```

Meaning when you get a command you can perform arbitrary side effects in IO, but in this stage you do not have access to the current model. Instead you have to return a continuation telling the library how to generate events or errors based on the model.

The reason for this approach is that it allows the library to guarantee that the model isn't updated where it should not be.


### Generating the server

Now we have defined the core parts of our domain. We can now generate the server using the template-haskell function `mkCmdServer`. It will generate a Servant API and server implementation. The only thing you need to do is to supply a `ActionRunner`.

To get a command runner we first have to define a persistance model, which tells us how the events are to be stored (note that the state is not persisted). We can then pick a start state and tie all the things together using `createModel`.

### Simple example

Minimal example can be found in [examples/simple/Main.hs](examples/simple/Main.hs).

For a slightly more realistic example check out [examples/hierarchical/Main.hs](examples/hierarchical/Main.hs).
