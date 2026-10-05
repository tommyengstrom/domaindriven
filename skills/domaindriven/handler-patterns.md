# Handler Patterns

Reusable patterns for server handlers in domaindriven applications.

## The `withX` Entity Handler Pattern

The most important boilerplate-killer in a real app. Every entity type gets a `withX` helper that handles 404s, runs the transaction, and returns the updated entity.

### Base version

```haskell
-- Look up entity, 404 if missing, run callback to produce events, return updated entity
withBook
    :: (Aggregate LibraryDomain Effectful.:> es, Error ServerError Effectful.:> es)
    => BookId -> (Book -> Eff es [LibraryEvent]) -> Eff es Book
withBook bid mkEvents = do
    result <- runTransaction @LibraryDomain \m -> do
        book <- lookupBook bid m
        evts <- mkEvents book
        pure (lookupBookPure bid, evts)
    either throwError pure result
```

### Composed child version

Child entity handlers validate both parent and child in a single transaction:

```haskell
withChapter
    :: (Aggregate LibraryDomain Effectful.:> es, Error ServerError Effectful.:> es)
    => BookId -> ChapterId
    -> (Book -> Chapter -> Eff es [LibraryEvent])
    -> Eff es Chapter
withChapter bid cid mkEvents = do
    result <- runTransaction @LibraryDomain \m -> do
        book <- lookupBook bid m
        chapter <- lookupChapter book cid
        evts <- mkEvents book chapter
        pure (lookupChapterPure bid cid, evts)
    either throwError pure result
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

Use effectful lookups to validate user input before emitting events, returning 404 when an entity is missing. Pure transaction result callbacks return `Either ServerError Entity`, which the handler unwraps after `runTransaction`.

An unexpectedly missing result becomes HTTP 500. This happens after commit, so reporting the failure does not roll back the events. Delete handlers should return `const NoContent` from their transaction rather than look up the deleted entity.

```haskell
-- Eff variant: validates user input, throws 404
lookupBook :: Error ServerError Effectful.:> es => BookId -> LibraryModel -> Eff es Book
lookupBook bid m =
    case Map.lookup bid m.books of
        Just b  -> pure b
        Nothing -> throwError err404{errBody = "Book not found"}

-- Pure variant: for returnFn after events applied
lookupBookPure :: BookId -> LibraryModel -> Either ServerError Book
lookupBookPure bid m =
    maybe (Left err500) Right (Map.lookup bid m.books)

lookupChapterPure :: BookId -> ChapterId -> LibraryModel -> Either ServerError Chapter
lookupChapterPure bid cid m = do
    book <- lookupBookPure bid m
    maybe (Left err500) Right (Map.lookup cid book.chapters)
```

## `setField` Helper

Generic field updates with equality check to skip redundant events. Essential because the event store is append-only — every event adds permanent storage cost:

```haskell
setBookField
    :: (Aggregate LibraryDomain Effectful.:> es, Error ServerError Effectful.:> es, Eq a)
    => BookId -> (Book -> a) -> (a -> BookEvent) -> a -> Eff es Book
setBookField bid getField mkEvent newValue = do
    result <- runTransaction @LibraryDomain \m -> do
        book <- lookupBook bid m
        if getField book == newValue
            then pure (const (Right book), [])
            else pure (lookupBookPure bid, [wrapBookE bid (mkEvent newValue)])
    either throwError pure result
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
