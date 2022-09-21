# DomainDriven

DomainDriven is a batteries included synchronous eventsourcing and CQRS library. The goal of this library is to allow you to implement DDD principles without focusing on the boilerplate. Using `Template Haskell` we generate a Servant server from the specification and we keep the specification succinct.

## The idea

- Use a GADT to specify the actions, what will be translated into `GET`s and `POST`s.
- Make each event update run in a transaction, thereby avoiding the eventual consistency issues commonly associated with eventsourcing.

## How it works

In order to implement a model in `domain-driven` you have to define:
- The model (current state)
- The events
- How to handle events
- The actions (queries and commands)
- How to handle actions

### Model

The model is the current state of the system. This is what you normally would keep in a database, but as this is an eventsources system the state is not fundamental as it can be recalculated.

### Events

Events are things that happened in the past. The event you define represent all the changes that can occur in the system.

Events should be specified in past tens.
```haskell

data Event
    = IncreasedCounter
    | DecreasedCounter
```

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
data StorageAction method a where
    GetFile :: UUID -> StorageAction Query ByteString
    AddFile :: ByteString -> StorageAction Cmd UUID
    RemoveFile :: UUID -> StorageAction Cmd ()
```

### Command handler

Commands, in contrast to events, are allowed to fail. If a command succeeds we need to return a value of the type specified by the constructor and a list of events. The command handler do not update the state.

In addition you may need to make requests, read from disk, or perform other side effects in order to calculate the result.

`ActionHandler` is defined as:

``` haskell
type ActionHandler model event m cmd
    = forall method a . cmd method a -> HandlerType method model event m a
```

In practice this means you specify actions as

```haskell

data CounterAction method return where
   GetCounter ::CounterAction Query Int
   IncreaseCounter ::CounterAction Cmd Int
   DecreaseCounter ::CounterAction Cmd Int
```

and the corresponding handler as

```haskell
handleAction
    :: CounterAction method a -> HandlerType method CounterModel CounterEvent IO a
handleAction = \case
    GetCounter      -> Query $ pure
    IncreaseCounter -> Cmd $ \_ -> pure (id, [CounterIncreased])
    DecreaseCounter -> Cmd $ \counter -> do
        when (counter < 1) (throwM NegativeNotSupported)
        pure (id, [CounterDecreased])

```

A `Query` takes a `model -> m a`, i.e. you get access to the model and the ability to run monadic efficts. `Query`s will be translates into `GET` in the generated API.

A `Cmd` has the additional ability of emitting events. It takes a `model -> m (model -> a, [event])`. The return value is specified as a function from the updated model to the return type. This way we can, in the Counter example, return the new value after the event handler has run.


### Generating the server

Now we have defined the core parts of our domain. We can now generate the server using the template-haskell function `mkCmdServer`. It will generate a Servant API and server implementation. The only thing you need to do is to supply a `ActionRunner`.

To get a command runner we first have to define a persistance model, which tells us how the events are to be stored (note that the state is not persisted). We can then pick a start state and tie all the things together using `createModel`.

### Simple example

Minimal example can be found in [examples/simple/Main.hs](examples/simple/Main.hs).

For a slightly more realistic example check out [examples/hierarchical/Main.hs](examples/hierarchical/Main.hs).
