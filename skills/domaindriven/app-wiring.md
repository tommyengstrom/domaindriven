# Application Wiring

How to wire up effects, backends, configuration, and tests.

## Effects Type Alias

Define an `Effects` type alias constraining the effect stack. Import Effectful's `(:>)` qualified to avoid collision with Servant's `(:>)`:

```haskell
import DomainDriven (Aggregate, GenId, Projection, genId)
import Effectful qualified
import Effectful.Error.Static (Error)

type Effects es =
    ( Projection LibraryDomain Effectful.:> es
    , Aggregate LibraryDomain Effectful.:> es
    , Error ServerError Effectful.:> es
    , GenId Effectful.:> es
    )
```

Then use it in handler signatures:

```haskell
myHandler :: Effects es => LibraryApi (AsServerT (Eff es))

createBook :: Effects es => CreateBook -> Eff es Book
createBook cmd = do
    bid <- BookId <$> genId
    runTransaction @LibraryDomain \_model ->
        pure (lookupBookPure bid, [wrapBookE bid BookAdded{title = cmd.title, author = cmd.author}])
```

`GenId` keeps application ID generation testable without exposing `IOE` throughout handler code. The concrete runner stack still contains `IOE`, because the `Aggregate`, `Projection`, and production `GenId` interpreters perform IO.

## Effect Stack Ordering

The type-level effect list and the interpreter chain are in **opposite** order. Effects listed first in the type are peeled last by interpreters:

```haskell
-- Type-level list (outermost first):
type AppM = Eff
    '[ Projection LibraryDomain   -- outermost: peeled last
     , Aggregate LibraryDomain
     , Error ServerError          -- must be above domain effects so handlers can throw
     , Reader.Reader Config       -- available to all effects above
     , GenId                      -- interpreted at the outer IO boundary
     , IOE                        -- innermost: always at the bottom
     ]

-- Interpreter chain (innermost first):
runEffectStack m =
    runEff                            -- IOE
        . runGenId                    -- GenId
        . Reader.runReader config     -- Reader
        . runErrorNoCallStack         -- Error
        . runAggregate backend        -- Aggregate
        . runProjection backend       -- Projection
        $ m
```

**Key rules:**

- `Error ServerError` must be peeled *after* domain effects so that transaction callbacks can throw servant errors.
- Keep `IOE` in the concrete `AppM` stack, but omit it from application `Effects` constraints unless a handler performs unrelated IO directly. Interpret `GenId` with `runGenId` immediately before `runEff`.

## `AnyWriteModel` — Backend Polymorphism

`AnyWriteModel` is an existential wrapper in `domaindriven-core` that hides the concrete backend type. Use it in your `Config` so the same code works with both production (Postgres) and test (ForgetfulInMemory) backends:

```haskell
import DomainDriven (AnyWriteModel(..))

data Config = Config
    { port :: Int
    , backend :: AnyWriteModel MyModel MyEvent NoIndex
    }

-- Production:
pgBackend <- postgresWriteModel pool eventTable applyEvent initialModel
let config = Config{port = 8080, backend = AnyWriteModel pgBackend}

-- Tests:
testBackend <- createForgetful applyEvent initialModel
let config = Config{port = 0, backend = AnyWriteModel testBackend}
```

When running interpreters, pattern-match to unwrap:

```haskell
case config.backend of
    AnyWriteModel be -> runAggregate be . runProjection be $ m
```

## Config and `runEffectStack`

Full wiring with `Reader Config`:

```haskell
import Effectful.Reader.Static qualified as Reader

runEffectStack :: AnyWriteModel MyModel MyEvent NoIndex -> Config -> AppM a -> Handler a
runEffectStack (AnyWriteModel backend) config m = do
    result <- liftIO
        $ runEff
        . runGenId
        . Reader.runReader config
        . runErrorNoCallStack @ServerError
        . runAggregate backend
        . runProjection backend
        $ m
    either throwError pure result
```

## Testing with `createForgetful`

### Basic test helpers

```haskell
import DomainDriven.Persistance.ForgetfulInMemory (createForgetful)

runTest
    :: Eff '[Projection MyDomain, Aggregate MyDomain, Error ServerError, GenId, IOE] a
    -> IO (Either ServerError a)
runTest action = do
    backend <- createForgetful applyEvent initialModel
    runEff
        . runGenId
        . runErrorNoCallStack @ServerError
        . runAggregate backend
        . runProjection backend
        $ action

runTestOrFail
    :: Eff '[Projection MyDomain, Aggregate MyDomain, Error ServerError, GenId, IOE] a
    -> IO a
runTestOrFail action = runTest action >>= either (fail . show) pure
```

For deterministic tests, replace `runGenId` with `runGenIdWith (pure fixedUuid)`. The supplied action runs once per `genId` request, so it can also be backed by `State` to provide a sequence of IDs.

### WAI integration test (with hspec)

```haskell
withTestEnv :: (Wai.Application -> IO ()) -> IO ()
withTestEnv action = do
    backend <- createForgetful applyEvent initialModel
    let app = mkApp (AnyWriteModel backend) testConfig
    action app

spec :: Spec
spec = around withTestEnv $ do
    it "creates a book" $ \app -> do
        resp <- postJSON app "/create" (encode CreateBook{title = "Test"})
        assertStatus 200 resp
```

## Multiple Domains

When your app has multiple domains (e.g. EditorDomain + GameDomain), put **one** domain in the Servant effect stack and run others separately:

```haskell
-- Servant monad carries EditorDomain only:
type AppM = Eff
    '[ Projection EditorDomain, Aggregate EditorDomain
     , Error ServerError, Reader.Reader Config, GenId, IOE
     ]

-- GameDomain runs its own stack where needed (e.g. WebSocket handler):
case config.gameBackend of
    AnyWriteModel backend ->
        runEff
            . runGenId
            . runAggregate @_ @GameDomain backend
            . runProjection @_ @GameDomain backend
            $ gameLoop
```

Each domain gets its own `type XDomain = Domain XModel XEvent XIndex`. Include and interpret `GenId` in a standalone stack only when that action generates application IDs; every concrete backend stack still retains `IOE`.

## Bootstrap / Seed Data

To populate an event-sourced database from JSON or other sources, decompose aggregates into granular events — don't emit one fat creation event.

### `diffField` helper

Emit an event only if a value changed (useful for syncing spec files):

```haskell
diffField :: Eq a => (ev -> TopEvent) -> a -> a -> (a -> ev) -> [TopEvent]
diffField wrap new old mkEvent
    | new == old = []
    | otherwise  = [wrap (mkEvent new)]
```

### Aggregate-to-events decomposition

```haskell
bookToEvents :: BookId -> Book -> [LibraryEvent]
bookToEvents bid book =
    let wrap = wrapBookE bid
    in  [wrap BookAdded{title = book.title, author = book.author}]
        <> [wrap SubtitleChanged{subtitle = s} | Just s <- [book.subtitle]]
        <> concatMap (chapterToEvents bid) (Map.toList book.chapters)
```

### Idempotent bootstrap

Check existing state before importing:

```haskell
existingModel <- getModel backend NoIndex
forM_ entries $ \(bid, book) ->
    unless (Map.member bid existingModel.books) $
        emitEvents backend (bookToEvents bid book)
```
