# Event Design Principles

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

Use a sum-of-sums pattern for the top-level event type:

```haskell
data AccountEvent
    = ProfileEvent { profileEvent :: ProfileEvent }
    | BillingEvent { billingEvent :: BillingEvent }
    deriving (Generic, ToJSON, FromJSON)

data ProfileEvent
    = ProfileCreated { email :: Text }
    | NameChanged { name :: Text }
    | PhoneNumberChanged { phone :: Text }
    deriving (Generic, ToJSON, FromJSON)

data BillingEvent
    = PlanSelected { plan :: Plan }
    | PaymentMethodAdded { method :: PaymentMethod }
    deriving (Generic, ToJSON, FromJSON)
```

Apply events with nested pattern matching:

```haskell
applyEvent :: AccountModel -> Stored AccountEvent -> AccountModel
applyEvent m (Stored evt _ _) = case evt of
    ProfileEvent pe -> m { profile = applyProfileEvent (profile m) pe }
    BillingEvent be -> m { billing = applyBillingEvent (billing m) be }

applyProfileEvent :: Profile -> ProfileEvent -> Profile
applyProfileEvent p = \case
    ProfileCreated email   -> p { email = email }
    NameChanged name       -> p { name = name }
    PhoneNumberChanged ph  -> p { phone = ph }
```

### Benefits

1. **Overview** — the top-level type shows the domain shape at a glance. Reading `AccountEvent` tells you the system has profile and billing concerns without scrolling through dozens of constructors.

2. **Migration** — `shapeCoerce` handles unchanged sub-types automatically. If only `BillingEvent` changed between versions, you only write a manual `ShapeCoercible` instance for `BillingEvent`; `ProfileEvent` passes through via the generic instance.

## Events in a Separate Package

Keep event types in a dedicated `<project>-events` package. This lets the migration package import frozen snapshots of events at each version while the events package stays editable. Without the split, you can't have two versions of the same module in scope at once. See [project-setup.md](project-setup.md) for the full 3-package workflow.
