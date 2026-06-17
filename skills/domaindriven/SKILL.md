---
name: domaindriven
description: Help users build applications with the domaindriven Haskell event sourcing library
---

You are an expert in the `domaindriven` Haskell library. Follow these patterns exactly.

## Architecture

Synchronous event sourcing with Effectful effects and Servant APIs.

Dependencies: `domaindriven-core` (persistence backends), `domaindriven` (Effectful API layer), `shape-coerce` (event migration).

## Core Pattern

### File layout

- `Types.hs` — ID newtypes, enumerations, entity records
- `Event.hs` — Hierarchical event types
- `Model.hs` — Model type, `emptyCrmModel`, Domain type alias
- `EventHandler.hs` — `applyEvent` with optics-based dispatch
- `Command.hs` — Request body types (one per mutation endpoint)
- `Api.hs` — Servant API types with `FieldNameAsPath`
- `Server.hs` — Handlers, `Effects` alias, `withX` helpers, event wrappers
- `Main.hs` — Entry point, backend creation, effect stack wiring

For larger apps, split `Api.hs` and `Server.hs` by sub-domain (e.g. `Api/Books.hs`, `Server/Books.hs`).

### Running example: Library with Books

All patterns below use a Library/Book domain — a `Map BookId Book` inside a Library model, with hierarchical events and nested API with Captures.

```haskell
-- 1. Domain triple
type LibraryDomain = Domain LibraryModel LibraryEvent NoIndex

-- 2. Model as Map (the natural pattern for entity collections)
data LibraryModel = LibraryModel
    { books :: Map BookId Book
    }
    deriving stock (Show, Eq, Generic)

emptyLibraryModel :: LibraryModel
emptyLibraryModel = LibraryModel mempty

-- 3. Hierarchical events with entity IDs (see event-design.md)
--    Every event type must derive NFData (import Control.DeepSeq (NFData)).
data LibraryEvent
    = BookEvent { bookId :: BookId, bookEvent :: BookEvent }
    deriving (Generic, ToJSON, FromJSON, NFData)

data BookEvent
    = BookAdded { title :: Text, author :: Text }
    | BookRemoved
    | TitleChanged { title :: Text }
    deriving (Generic, ToJSON, FromJSON, NFData)

-- 4. Apply events with optics (see event-design.md for full dispatch)
applyEvent :: LibraryModel -> Stored LibraryEvent -> LibraryModel
applyEvent model (Stored (BookEvent{bookId = bid, bookEvent = be}) _ _) =
    applyBookEvent bid be model

applyBookEvent :: BookId -> BookEvent -> LibraryModel -> LibraryModel
applyBookEvent bid ev model = case ev of
    BookAdded{title, author} ->
        model & #books % at bid .~ Just Book{bookId = bid, title, author}
    BookRemoved ->
        model & #books % at bid .~ Nothing
    TitleChanged{title} ->
        model & #books % ix bid % #title .~ title

-- 5. Create backend
-- Testing:
backend <- createForgetful applyEvent emptyLibraryModel
-- Production:
pool <- simplePool' connectInfo
backend <- postgresWriteModel pool eventTable applyEvent emptyLibraryModel

-- 6. Handlers use withX pattern + Effects alias (see handler-patterns.md)
-- 7. Wire effect stack (see app-wiring.md)
```

## Commands

Commands are request-body types — what the client *wants* to happen. They are separate from events (what *did* happen). Define one per mutation endpoint in `Command.hs`:

```haskell
data CreateBook = CreateBook { title :: Text, author :: Text }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

newtype ChangeTitle = ChangeTitle { title :: Text }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
```

Use `newtype` for single-field commands, `data` for multi-field.

## Event Design

Keep events small (one fact per event), use hierarchical sum-of-sums for the top-level event type, and put events in a separate package for migration safety. See [event-design.md](event-design.md) for principles, optics-based dispatch, and the optics cheat sheet.

## Key Types

```haskell
data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    }

data Domain (model :: Type) (event :: Type) (index :: Type) = Domain

data NoIndex = NoIndex
newtype Indexed = Indexed Text

-- Backend polymorphism (in domaindriven-core):
data AnyWriteModel model event index
    = forall p. (WriteModel p, Model p ~ model, Event p ~ event, Index p ~ index)
    => AnyWriteModel p

data Aggregate (domain :: Type) :: Effect where
    RunTransactionI
        :: DomainIndex domain
        -> (DomainModel domain -> Eff es (DomainModel domain -> a, [DomainEvent domain]))
        -> Aggregate domain (Eff es) a

data Projection domain :: Effect where
    GetModelI    :: DomainIndex domain -> Projection domain m (DomainModel domain)
    GetEventListI :: DomainIndex domain -> Projection domain m [Stored (DomainEvent domain)]
```

## Transaction Conventions

Each endpoint should use a single `runTransaction` call unless there's a strong reason not to. The Postgres backend holds an exclusive advisory lock for the entire transaction, making the read-validate-emit cycle atomic — no other request can interleave.

**When to split**: If the endpoint must do time-consuming work (e.g. external API calls) that would hold the lock too long, do the slow part outside `runTransaction`, then call `runTransaction` to emit events.

### Return Convention

The callback returns `(Model -> a, [Event])`:
- First element extracts return value from the *updated* model (after events applied)
- `const ()` when nothing to return, `id` to return whole model, `(.someField)` for a field
- **Important**: the `returnFn` is a pure function — no `Eff` effects available. Use pure lookup helpers here (see [handler-patterns.md](handler-patterns.md)).

## Indexed Aggregates

Use `runTransactionI` and `getModelI` with an index:

```haskell
type MyDomain = Domain MyModel MyEvent Indexed
increase :: Indexed -> Eff es Int
increase idx = runTransactionI idx \model -> pure ((.counter), [Increased])
```

## Servant Integration

### FieldNameAsPath basics

`FieldNameAsPath` derives URL paths from record field names. Each field name becomes a path segment automatically:

```haskell
data CounterAPI mode = CounterAPI
    { get      :: mode :- Get '[JSON] Int       -- GET  /get
    , increase :: mode :- Post '[JSON] Int      -- POST /increase
    , decrease :: mode :- Post '[JSON] Int      -- POST /decrease
    } deriving Generic

instance ApiTagFromLabel CounterAPI
```

Serve with:

```haskell
serve (Proxy @(FieldNameAsPathApi CounterAPI))
    $ hoistServer (Proxy @(FieldNameAsPathApi CounterAPI)) runEffectStack
    $ FieldNameAsPathServer counterHandler
```

### Nested APIs with Captures

For real applications, nest API records using `Capture` + `FieldNameAsPathApi`. Use `list`/`create` plus a resource-named capture field at collection levels, and `get` for single-entity retrieval. Field names in API record types are record selectors inside a named type — they do **not** clash with Prelude, so no trailing underscores are needed.

```haskell
-- Collection level: /books
data BooksApi mode = BooksApi
    { list   :: mode :- Get '[JSON] [Book]
    , create :: mode :- ReqBody '[JSON] CreateBook :> Post '[JSON] Book
    , book   :: mode :- Capture "bookId" BookId :> FieldNameAsPathApi BookApi
    } deriving stock Generic

instance ApiTagFromLabel BooksApi

-- Entity level: /books/{bookId}/...
data BookApi mode = BookApi
    { get         :: mode :- Get '[JSON] Book
    , changeTitle :: mode :- ReqBody '[JSON] ChangeTitle :> Post '[JSON] Book
    , remove      :: mode :- Delete '[JSON] NoContent
    , chapters    :: mode :- FieldNameAsPathApi ChaptersApi   -- nest further
    } deriving stock Generic

instance ApiTagFromLabel BookApi
```

Nested capture fields become URL path segments, so name them after the resource (`book`, `chapter`, …) — never a generic word like `detail`, which would produce meaningless URLs like `/detail/{bookId}/...`.

Each nested level needs its own `instance ApiTagFromLabel`. The handler nesting mirrors the API type:

```haskell
booksServer :: Effects es => BooksApi (AsServerT (Eff es))
booksServer = BooksApi
    { list   = Map.elems . (.books) <$> getModel @LibraryDomain
    , create = \cmd -> ...
    , book   = \bid -> FieldNameAsPathServer $ bookServer bid
    }

bookServer :: Effects es => BookId -> BookApi (AsServerT (Eff es))
bookServer bid = BookApi
    { get         = getModel @LibraryDomain >>= lookupBook bid
    , changeTitle = \cmd -> withBook bid \_book ->
          pure [wrapBookE bid TitleChanged{title = cmd.title}]
    , remove      = ...
    , chapters    = FieldNameAsPathServer $ chaptersServer bid
    }
```

### Auth Headers

When an API needs auth, declare the header as **required** at the API type level so handlers receive a non-`Maybe` token. Reject missing or invalid credentials at the API boundary (throwing 401 there), not deep in business logic — otherwise `Maybe Token` propagates through every handler and 401 handling leaks into domain code.

## Handler Patterns

See [handler-patterns.md](handler-patterns.md) for:
- `withX` entity handler pattern (base + composed child versions)
- Lookup helpers (Eff + Pure variants)
- `setField` helper for generic field updates
- Event wrapping helpers
- Create with optional events
- Validation (`validateNotBlank`, `blankToNothing`)

## Application Wiring

See [app-wiring.md](app-wiring.md) for:
- `Effects` type alias with qualified `Effectful.:>`
- Effect stack ordering (type list vs interpreter chain)
- `AnyWriteModel` backend polymorphism
- Config and `runEffectStack`
- Testing with `createForgetful`
- Multiple domains
- Bootstrap / seed data

## Event Migration (Postgres)

Use `shape-coerce` for migrations. Let the compiler guide you:

1. First, try just `shapeCoerce`. If the old and new event types are structurally identical (same constructor names, same fields), it works automatically via Generics.

2. If the types differ, the compiler will tell you exactly what doesn't match. Write a manual `ShapeCoercible` instance:

```haskell
instance ShapeCoercible V1.CounterEvent V2.CounterEvent where
    shapeCoerce = \case
        V1.CounterIncreased -> V2.CounterIncreasedBy 1
        V1.CounterDecreased -> V2.CounterDecreasedBy 1
```

3. Wire it into the migration chain:

```haskell
eventTable :: EventTable
eventTable = MigrateUsing myMigration $ InitialVersion "my_events"

myMigration :: EventMigration
myMigration prev next conn = migrate1to1 @NoIndex conn prev next shapeCoerce
```

For multi-package project setup with compile-time migration safety, see [project-setup.md](project-setup.md).

## Imports

```haskell
import DomainDriven                                        -- re-exports Aggregate, Projection, interpreters, Stored, NoIndex, AnyWriteModel
import DomainDriven.FieldNameAsPath (ApiTagFromLabel, FieldNameAsPathApi, FieldNameAsPathServer(..))
import DomainDriven.Persistance.Class (mkId)               -- UUID generation
import DomainDriven.Persistance.ForgetfulInMemory (createForgetful)  -- testing backend
import DomainDriven.Persistance.Postgres (postgresWriteModel, simplePool')  -- production backend

-- Effectful (qualified to avoid Servant collision):
import Effectful hiding ((:>))
import Effectful qualified
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static qualified as Reader          -- for Config pattern

-- Servant:
import Servant hiding (throwError)
import Servant qualified
import Servant.Server.Generic (AsServerT)

-- Optics (for applyEvent):
import Optics.Core (at, ix, (%))
import Optics.Operators ((.~))
import Data.Function ((&))
```
