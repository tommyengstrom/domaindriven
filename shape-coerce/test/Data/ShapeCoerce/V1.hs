module Data.ShapeCoerce.V1 where

import GHC.Generics (Generic)
import Prelude

data InsertAtStart
    = StartFirst
    | StartSecond Int
    | StartThird Bool
    deriving stock (Eq, Show, Generic)

data InsertAfterFirst
    = AfterFirstFirst
    | AfterFirstSecond Int
    | AfterFirstThird Bool
    deriving stock (Eq, Show, Generic)

data InsertAfterSecond
    = AfterSecondFirst
    | AfterSecondSecond Int
    | AfterSecondThird Bool
    deriving stock (Eq, Show, Generic)

data InsertAtEnd
    = EndFirst
    | EndSecond Int
    | EndThird Bool
    deriving stock (Eq, Show, Generic)

data InsertConsecutively
    = ConsecutiveFirst
    | ConsecutiveSecond String
    | ConsecutiveThird Bool
    deriving stock (Eq, Show, Generic)

data InsertInMultiplePlaces
    = MultipleFirst
    | MultipleSecond Int
    | MultipleThird Bool
    deriving stock (Eq, Show, Generic)

data InsertBeforeSingleton
    = BeforeSingletonOriginal Int
    deriving stock (Eq, Show, Generic)

data InsertAfterSingleton
    = AfterSingletonOriginal String
    deriving stock (Eq, Show, Generic)

data EqualSum
    = EqualFirst
    | EqualSecond Int
    | EqualThird Bool
    deriving stock (Eq, Show, Generic)
