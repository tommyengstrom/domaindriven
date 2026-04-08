# Handler Patterns

Reusable patterns for server handlers in domaindriven applications.

## The `withX` Entity Handler Pattern

The most important boilerplate-killer in a real app. Every entity type gets a `withX` helper that handles 404s, runs the transaction, and returns the updated entity.

### Base version

```haskell
-- Look up entity, 404 if missing, run callback to produce events, return updated entity
withBook :: Effects es => BookId -> (Book -> Eff es [LibraryEvent]) -> Eff es Book
withBook bid mkEvents = runTransaction @LibraryDomain \m -> do
    book <- lookupBook bid m
    evts <- mkEvents book
    pure (\m' -> lookupBookPure bid m', evts)
```

### Composed child version

Child entity handlers delegate to the parent. This ensures both parent and child are validated in a single transaction:

```haskell
withChapter
    :: Effects es
    => BookId -> ChapterId
    -> (Book -> Chapter -> Eff es [LibraryEvent])
    -> Eff es Chapter
withChapter bid cid mkEvents = runTransaction @LibraryDomain \m -> do
    book <- lookupBook bid m
    chapter <- lookupChapter book cid
    evts <- mkEvents book chapter
    pure (\m' -> lookupChapterPure bid cid m', evts)
```

### Usage becomes trivial

```haskell
changeTitle = \cmd ->
    withBook bid \_book ->
        pure [wrapBookE bid TitleChanged{title = cmd.title}]

changeChapterTitle = \cmd ->
    withChapter bid cid \_book _chapter ->
        pure [wrapChapE bid cid ChapterTitleChanged{title = cmd.title}]
```

## Lookup Helpers (Eff + Pure Variants)

You always need **two variants** of every lookup:

1. **Eff variant** — throws 404, used inside `runTransaction`'s action to validate user input
2. **Pure variant** — uses `error` for invariant violations, used in the `returnFn` callback

### Why two variants

`runTransaction` returns `(Model -> a, [Event])` where the first element runs *after* events are applied. Inside that callback:
- The entity is guaranteed to exist (you just created/updated it)
- You can't use `Eff` effects (it's a pure function `Model -> a`)
- An invariant `error` is correct — if the entity is missing after your own events, that's a bug

```haskell
-- Eff variant: validates user input, throws 404
lookupBook :: Error ServerError Effectful.:> es => BookId -> LibraryModel -> Eff es Book
lookupBook bid m =
    case Map.lookup bid m.books of
        Just b  -> pure b
        Nothing -> throwError err404{errBody = "Book not found"}

-- Pure variant: for returnFn after events applied
lookupBookPure :: BookId -> LibraryModel -> Book
lookupBookPure bid m =
    case Map.lookup bid m.books of
        Just b  -> b
        Nothing -> error "Invariant violation: book not found after transaction"
```

## `setField` Helper

Generic field updates with equality check to skip redundant events. Essential because the event store is append-only — every event adds permanent storage cost:

```haskell
setBookField
    :: (Effects es, Eq a)
    => BookId -> (Book -> a) -> (a -> BookEvent) -> a -> Eff es Book
setBookField bid getField mkEvent newValue =
    runTransaction @LibraryDomain \m -> do
        book <- lookupBook bid m
        if getField book == newValue
            then pure (const book, [])
            else pure (\m' -> lookupBookPure bid m', [wrapBookE bid (mkEvent newValue)])
```

Usage:

```haskell
setTitle = \cmd -> setBookField bid (.title) (\t -> TitleChanged{title = t}) cmd.title
```

## Event Wrapping Helpers

Define composable wrapping helpers that mirror the domain hierarchy. Each helper takes IDs for its level plus the inner event and delegates to the parent:

```haskell
wrapBookE :: BookId -> BookEvent -> LibraryEvent
wrapBookE bid be = BookEvent{bookId = bid, bookEvent = be}

wrapChapE :: BookId -> ChapterId -> ChapterEvent -> LibraryEvent
wrapChapE bid cid ce = wrapBookE bid ChapterEvent{chapterId = cid, chapterEvent = ce}
```

This keeps event construction in handlers clean and composable.

## Create with Optional Events

For creation endpoints where some fields are optional, emit the required creation event, then conditionally append field-setting events using list comprehension guards:

```haskell
let wrap = wrapBookE bid
    events =
        [wrap BookAdded{title = cmd.title, author = cmd.author}]
        <> [wrap SubtitleChanged{subtitle = s} | Just s <- [cmd.subtitle]]
        <> [wrap IsbnChanged{isbn = i}         | Just i <- [cmd.isbn]]
```

This keeps optional fields out of the creation event and reuses the same field-change events that update endpoints use.

## Validation

Validate before writing — in an event-sourced system, bad data is permanent.

```haskell
validateNotBlank :: Error ServerError Effectful.:> es => Text -> Eff es ()
validateNotBlank t
    | Text.null (Text.strip t) = throwError err400{errBody = "Value cannot be blank"}
    | otherwise = pure ()

-- Normalize optional text fields before emitting events
blankToNothing :: Maybe Text -> Maybe Text
blankToNothing (Just t) | Text.null (Text.strip t) = Nothing
blankToNothing x = x
```
