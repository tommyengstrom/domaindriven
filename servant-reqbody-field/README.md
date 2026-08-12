# servant-reqbody-field

`servant-reqbody-field` provides a Servant combinator that turns fields from one
JSON object request body into separate handler arguments:

```haskell
type AddUser =
    ReqBodyField "firstName" Text
        :> ReqBodyField "lastName" Text
        :> Post '[JSON] User

addUser :: Text -> Text -> Handler User
```

All `ReqBodyField` combinators in an endpoint share one request-local parse of
the body, even when headers, query parameters, descriptions, or other
non-body combinators appear between them. Undeclared object properties are
ignored.

The body and its `Content-Type: application/json` header are required.
`ReqBodyField name (Maybe a)` treats an omitted property or JSON `null` as
`Nothing`; generated clients omit that property for `Nothing` while still
sending an object body. Duplicate declarations decode the property at every
declared type. Generated clients keep the last non-`Nothing` value for a
duplicate key.

## Request-body ownership

Only `ReqBodyField` combinators participate in the shared parsed-body replay.
The first one strictly consumes the request stream and caches the decoded JSON
value for the remaining `ReqBodyField` combinators. Combining them with another
body reader on the same endpoint is unsupported; this includes `ReqBody`,
`StreamBody`, and custom combinators that read the body independently.

`Raw` and `RawM` are safe after `ReqBodyField` only when the raw application
does not read the exhausted request body. Middleware that inspects a body must
either leave the stream unread or replace it with a replayable stream before
passing the request onward.

Servant alternatives with the same path and method cannot dispatch according
to body shape. If an earlier alternative reaches a `ReqBodyField` and its body
check fails, the fatal `400` response prevents Servant from trying a later
body-shaped alternative.

## Resource limits

Request bodies are buffered strictly in memory. Deploy applications with an
appropriate request-size limit and request timeout; this package deliberately
does not impose either policy itself.

## Dependency graphs

This repository's root project uses a `servant-openapi3` fork to support the
`insert-ordered-containers-0.3.0` version selected by its Stackage snapshot.
That override is not part of the package source distribution. Standalone
Hackage builds use stock `servant-openapi3-2.0.2.0` with
`insert-ordered-containers-0.2.7`.
