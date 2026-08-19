module Data.ShapeCoerce.V2 where

import GHC.Generics (Generic)
import Prelude

data InsertAtStart
    = StartAdded Char
    | StartFirst
    | StartSecond Int
    | StartThird Bool
    deriving stock (Eq, Show, Generic)

data InsertAfterFirst
    = AfterFirstFirst
    | AfterFirstAdded Char
    | AfterFirstSecond Int
    | AfterFirstThird Bool
    deriving stock (Eq, Show, Generic)

data InsertAfterSecond
    = AfterSecondFirst
    | AfterSecondSecond Int
    | AfterSecondAdded Char
    | AfterSecondThird Bool
    deriving stock (Eq, Show, Generic)

data InsertAtEnd
    = EndFirst
    | EndSecond Int
    | EndThird Bool
    | EndAdded Char
    deriving stock (Eq, Show, Generic)

data InsertConsecutively
    = ConsecutiveFirst
    | ConsecutiveAddedOne Double
    | ConsecutiveAddedTwo Char
    | ConsecutiveSecond String
    | ConsecutiveThird Bool
    deriving stock (Eq, Show, Generic)

data InsertInMultiplePlaces
    = MultipleAddedAtStart
    | MultipleFirst
    | MultipleAddedAfterFirst Char
    | MultipleSecond Int
    | MultipleThird Bool
    | MultipleAddedAtEnd Double
    deriving stock (Eq, Show, Generic)

data InsertBeforeSingleton
    = BeforeSingletonAdded
    | BeforeSingletonOriginal Int
    deriving stock (Eq, Show, Generic)

data InsertAfterSingleton
    = AfterSingletonOriginal String
    | AfterSingletonAdded
    deriving stock (Eq, Show, Generic)

data EqualSum
    = EqualFirst
    | EqualSecond Int
    | EqualThird Bool
    deriving stock (Eq, Show, Generic)
