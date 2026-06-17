# Event Design Principles

> **Required instances:** every event type (at every level of the hierarchy)
> must derive `NFData` in addition to `ToJSON`/`FromJSON`. The persistence layer
> forces parsed events with `deepseq`, and the requirement is enforced uniformly
> across all backends via a superclass on `ReadModel`. `deriving (Generic,
> ToJSON, FromJSON, NFData)` covers it (`import Control.DeepSeq (NFData)`).

## Small Events

Each event should capture exactly one fact. If a command does multiple things, emit multiple events.

**Bad** — one big event bundles unrelated facts:

```haskell
data PersonEvent = PersonCreated { email :: Text, phone :: Text }
```

**Good** — separate events for separate facts:

```haskell
data PersonEvent
    = PersonCreated { email :: Text }
    | PhoneNumberChanged { phone :: Text }
```

A `createPerson` handler emits both `[PersonCreated email, PhoneNumberChanged phone]` in a single transaction. This keeps each event reusable (phone changes later reuse `PhoneNumberChanged`) and makes migrations simpler (adding a field to `PersonCreated` doesn't affect phone logic).

## Hierarchical Events

Use a sum-of-sums pattern for the top-level event type. Each level wraps the next with an entity ID:

```haskell
-- Top-level event (what gets stored)
data LibraryEvent
    = BookEvent { bookId :: BookId, bookEvent :: BookEvent }
    deriving (Generic, ToJSON, FromJSON, NFData)

-- Book-level events
data BookEvent
    = BookAdded { title :: Text, author :: Text }
    | BookRemoved
    | TitleChanged { title :: Text }
    | AuthorChanged { author :: Text }
    | ChapterEvent { chapterId :: ChapterId, chapterEvent :: ChapterEvent }
    deriving (Generic, ToJSON, FromJSON, NFData)

-- Chapter-level events (3rd level)
data ChapterEvent
    = ChapterAdded { title :: Text, pageCount :: Int }
    | ChapterRemoved
    | ChapterTitleChanged { title :: Text }
    deriving (Generic, ToJSON, FromJSON, NFData)
```

### Benefits

1. **Overview** — the top-level type shows the domain shape at a glance.
2. **Migration** — `shapeCoerce` handles unchanged sub-types automatically. If only `BookEvent` changed between versions, you only write a manual `ShapeCoercible` instance for `BookEvent`.

> **Common mistake:** if an event carries an entity ID and only mutates that entity's state, it belongs nested under that entity's event type — not at the top level. Reserve top-level constructors for cross-cutting concerns that affect global state. A flat top-level event type is a smell: you lose the migration leverage that `shapeCoerce` gives you over unchanged sub-types, and the domain shape stops being visible at a glance.

## ID-Routed Dispatch with Optics

For hierarchical events, build a dispatch chain where each level pattern-matches on its event constructors and either applies the change via optics or delegates to the next level.

### Top-level dispatch

```haskell
applyEvent :: LibraryModel -> Stored LibraryEvent -> LibraryModel
applyEvent model (Stored (BookEvent{bookId = bid, bookEvent = be}) _ _) =
    applyBookEvent bid be model
```

### Entity-level dispatch with optics

Use `at key .~ Just val` for inserts, `at key .~ Nothing` for deletes, and `ix key % #field .~ val` for updates. `ix` silently does nothing if the key is missing — this is correct because events are historical facts and the entity may have been deleted.

```haskell
applyBookEvent :: BookId -> BookEvent -> LibraryModel -> LibraryModel
applyBookEvent bid ev model = case ev of
    BookAdded{title, author} ->
        model & #books % at bid .~ Just Book{bookId = bid, title, author, chapters = mempty}
    BookRemoved ->
        model & #books % at bid .~ Nothing
    TitleChanged{title} ->
        model & #books % ix bid % #title .~ title
    AuthorChanged{author} ->
        model & #books % ix bid % #author .~ author
    ChapterEvent{chapterId = cid, chapterEvent = ce} ->
        applyChapterEvent bid cid ce model
```

### Nested dispatch (3rd level)

For deeply nested entities, extract the optics path into a `let` binding for readability:

```haskell
applyChapterEvent :: BookId -> ChapterId -> ChapterEvent -> LibraryModel -> LibraryModel
applyChapterEvent bid cid ev model =
    let chapterPath = #books % ix bid % #chapters
    in case ev of
        ChapterAdded{title, pageCount} ->
            model & chapterPath % at cid .~ Just Chapter{chapterId = cid, title, pageCount}
        ChapterRemoved ->
            model & chapterPath % at cid .~ Nothing
        ChapterTitleChanged{title} ->
            model & chapterPath % ix cid % #title .~ title
```

### Silently ignore missing entities

Use `ix` (not `at`) when updating fields — `ix` is a no-op if the key doesn't exist. This is correct behaviour because `applyEvent` replays historical events. If an entity was created then deleted, later update events for it are irrelevant and should be silently skipped.

## Optics Cheat Sheet

All examples use the `optics` library with `%` for composition and `#field` via `OverloadedLabels`.

| Operation | Expression |
|-----------|-----------|
| Insert into Map | `model & #books % at bid .~ Just newBook` |
| Delete from Map | `model & #books % at bid .~ Nothing` |
| Update a field | `model & #books % ix bid % #title .~ newTitle` |
| Nested path | `model & #books % ix bid % #chapters % ix cid % #title .~ t` |
| Read (preview) | `preview (#books % ix bid) model` — returns `Maybe Book` |

**Key difference from `lens`:** optics uses `%` for composition (not `.`), and `#field` requires `OverloadedLabels` + a `Generic` instance on the record.

## Sum Type Models

If your model uses sum types (multiple constructors with named fields), `#field` via `OverloadedLabels` won't work because GHC's generic optics require single-constructor records. Use Template Haskell instead:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Optics.TH (makeFieldLabelsNoPrefix)

data Deductible
    = FlatDeductible { description :: Text, amount :: Money }
    | ProportionalDeductible { description :: Text, proportion :: Scientific }
    deriving stock (Generic)

makeFieldLabelsNoPrefix ''Deductible
-- Now #description works across both constructors (as an AffineTraversal)
```

## Events in a Separate Package

Keep event types in a dedicated `<project>-events` package. This lets the migration package import frozen snapshots of events at each version while the events package stays editable. Without the split, you can't have two versions of the same module in scope at once. See [project-setup.md](project-setup.md) for the full 3-package workflow.
