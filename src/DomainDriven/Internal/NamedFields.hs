{-# LANGUAGE OverloadedLists #-}

module DomainDriven.Internal.NamedFields where

import           Control.Lens                   ( (.~)
                                                , (?~)
                                                )
import           Data.Aeson
import           Data.OpenApi
import           DomainDriven.Internal.JsonFieldName
import           RIO                     hiding ( (.~) )

data NamedFields a = NamedFields
    { unNamedFields :: a
    }
    deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance {-# OVERLAPPABLE #-} ( JsonFieldName a, ToJSON a)
        => ToJSON (NamedFields a) where
    toJSON (NamedFields a) = Object [(fieldName @a, toJSON a)]

instance {-# OVERLAPPABLE #-} (JsonFieldName a, ToSchema a)
        => ToSchema (NamedFields a) where
    declareNamedSchema _ = do
        a <- declareSchemaRef $ Proxy @a
        pure
            $  NamedSchema Nothing
            $  mempty
            &  type_
            ?~ OpenApiObject
            &  properties
            .~ [(fieldName @a, a)]
instance
  {-# OVERLAPPABLE #-}
  ( JsonFieldName a, FromJSON a
  ) =>
  FromJSON (NamedFields a)
  where
    parseJSON =
        fmap (fmap NamedFields) . withObject ("NamedFields") $ \v -> v .: fieldName @a


instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON a,
    JsonFieldName b, ToJSON b
  ) =>
  ToJSON (NamedFields (a, b))
  where
    toJSON (NamedFields (a, b)) =
        Object [(fieldName @a, toJSON a), (fieldName @b, toJSON b)]

instance (JsonFieldName a, ToSchema a
         , JsonFieldName b, ToSchema b
         ) => ToSchema (NamedFields (a, b)) where
    declareNamedSchema _ = do
        a <- declareSchemaRef $ Proxy @a
        b <- declareSchemaRef $ Proxy @b
        pure
            $  NamedSchema Nothing
            $  mempty
            &  type_
            ?~ OpenApiObject
            &  properties
            .~ [(fieldName @a, a), (fieldName @b, b)]

instance
  ( JsonFieldName a, ToSchema a,
    JsonFieldName b, ToSchema b,
    JsonFieldName c, ToSchema c
  ) =>
  ToSchema (NamedFields (a, b, c))
  where
    declareNamedSchema _ = do
        a <- declareSchemaRef $ Proxy @a
        b <- declareSchemaRef $ Proxy @b
        c <- declareSchemaRef $ Proxy @c
        pure
            $  NamedSchema Nothing
            $  mempty
            &  type_
            ?~ OpenApiObject
            &  properties
            .~ [(fieldName @a, a), (fieldName @b, b), (fieldName @c, c)]

instance
  ( JsonFieldName a, ToSchema a,
    JsonFieldName b, ToSchema b,
    JsonFieldName c, ToSchema c,
    JsonFieldName d, ToSchema d
  ) =>
  ToSchema (NamedFields (a, b, c, d))
  where
    declareNamedSchema _ = do
        a <- declareSchemaRef $ Proxy @a
        b <- declareSchemaRef $ Proxy @b
        c <- declareSchemaRef $ Proxy @c
        d <- declareSchemaRef $ Proxy @d
        pure
            $  NamedSchema Nothing
            $  mempty
            &  type_
            ?~ OpenApiObject
            &  properties
            .~ [ (fieldName @a, a)
               , (fieldName @b, b)
               , (fieldName @c, c)
               , (fieldName @d, d)
               ]

instance
  ( JsonFieldName a, ToSchema a,
    JsonFieldName b, ToSchema b,
    JsonFieldName c, ToSchema c,
    JsonFieldName d, ToSchema d,
    JsonFieldName e, ToSchema e
  ) =>
  ToSchema (NamedFields (a, b, c, d, e))
  where
    declareNamedSchema _ = do
        a <- declareSchemaRef $ Proxy @a
        b <- declareSchemaRef $ Proxy @b
        c <- declareSchemaRef $ Proxy @c
        d <- declareSchemaRef $ Proxy @d
        e <- declareSchemaRef $ Proxy @e
        pure
            $  NamedSchema Nothing
            $  mempty
            &  type_
            ?~ OpenApiObject
            &  properties
            .~ [ (fieldName @a, a)
               , (fieldName @b, b)
               , (fieldName @c, c)
               , (fieldName @d, d)
               , (fieldName @e, e)
               ]

instance
  ( JsonFieldName a, ToSchema a,
    JsonFieldName b, ToSchema b,
    JsonFieldName c, ToSchema c,
    JsonFieldName d, ToSchema d,
    JsonFieldName e, ToSchema e,
    JsonFieldName f, ToSchema f
  ) =>
  ToSchema (NamedFields (a, b, c, d, e, f))
  where
    declareNamedSchema _ = do
        a <- declareSchemaRef $ Proxy @a
        b <- declareSchemaRef $ Proxy @b
        c <- declareSchemaRef $ Proxy @c
        d <- declareSchemaRef $ Proxy @d
        e <- declareSchemaRef $ Proxy @e
        f <- declareSchemaRef $ Proxy @f
        pure
            $  NamedSchema Nothing
            $  mempty
            &  type_
            ?~ OpenApiObject
            &  properties
            .~ [ (fieldName @a, a)
               , (fieldName @b, b)
               , (fieldName @c, c)
               , (fieldName @d, d)
               , (fieldName @e, e)
               , (fieldName @f, f)
               ]

instance
  ( JsonFieldName a, ToSchema a,
    JsonFieldName b, ToSchema b,
    JsonFieldName c, ToSchema c,
    JsonFieldName d, ToSchema d,
    JsonFieldName e, ToSchema e,
    JsonFieldName f, ToSchema f,
    JsonFieldName g, ToSchema g
  ) =>
  ToSchema (NamedFields (a, b, c, d, e, f, g))
  where
    declareNamedSchema _ = do
        a <- declareSchemaRef $ Proxy @a
        b <- declareSchemaRef $ Proxy @b
        c <- declareSchemaRef $ Proxy @c
        d <- declareSchemaRef $ Proxy @d
        e <- declareSchemaRef $ Proxy @e
        f <- declareSchemaRef $ Proxy @f
        g <- declareSchemaRef $ Proxy @g
        pure
            $  NamedSchema Nothing
            $  mempty
            &  type_
            ?~ OpenApiObject
            &  properties
            .~ [ (fieldName @a, a)
               , (fieldName @b, b)
               , (fieldName @c, c)
               , (fieldName @d, d)
               , (fieldName @e, e)
               , (fieldName @f, f)
               , (fieldName @g, g)
               ]

instance
  ( JsonFieldName a, ToSchema a,
    JsonFieldName b, ToSchema b,
    JsonFieldName c, ToSchema c,
    JsonFieldName d, ToSchema d,
    JsonFieldName e, ToSchema e,
    JsonFieldName f, ToSchema f,
    JsonFieldName g, ToSchema g,
    JsonFieldName h, ToSchema h
  ) =>
  ToSchema (NamedFields (a, b, c, d, e, f, g, h))
  where
    declareNamedSchema _ = do
        a <- declareSchemaRef $ Proxy @a
        b <- declareSchemaRef $ Proxy @b
        c <- declareSchemaRef $ Proxy @c
        d <- declareSchemaRef $ Proxy @d
        e <- declareSchemaRef $ Proxy @e
        f <- declareSchemaRef $ Proxy @f
        g <- declareSchemaRef $ Proxy @g
        h <- declareSchemaRef $ Proxy @h
        pure
            $  NamedSchema Nothing
            $  mempty
            &  type_
            ?~ OpenApiObject
            &  properties
            .~ [ (fieldName @a, a)
               , (fieldName @b, b)
               , (fieldName @c, c)
               , (fieldName @d, d)
               , (fieldName @e, e)
               , (fieldName @f, f)
               , (fieldName @g, g)
               , (fieldName @h, h)
               ]

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON  a, FromJSON a,
    JsonFieldName b, ToJSON  b, FromJSON b,
    JsonFieldName c, ToJSON  c, FromJSON c
  ) =>
  ToJSON (NamedFields (a, b, c))
  where
    toJSON (NamedFields (a, b, c)) = Object
        [(fieldName @a, toJSON a), (fieldName @b, toJSON b), (fieldName @c, toJSON c)]

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON  a, FromJSON a,
    JsonFieldName b, ToJSON  b, FromJSON b,
    JsonFieldName c, ToJSON  c, FromJSON c,
    JsonFieldName d, ToJSON  d, FromJSON d
  ) =>
  ToJSON (NamedFields (a, b, c, d))
  where
    toJSON (NamedFields (a, b, c, d)) = Object
        [ (fieldName @a, toJSON a)
        , (fieldName @b, toJSON b)
        , (fieldName @c, toJSON c)
        , (fieldName @d, toJSON d)
        ]

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON  a, FromJSON a,
    JsonFieldName b, ToJSON  b, FromJSON b,
    JsonFieldName c, ToJSON  c, FromJSON c,
    JsonFieldName d, ToJSON  d, FromJSON d,
    JsonFieldName e, ToJSON  e, FromJSON e
  ) =>
  ToJSON (NamedFields (a, b, c, d, e))
  where
    toJSON (NamedFields (a, b, c, d, e)) = Object
        [ (fieldName @a, toJSON a)
        , (fieldName @b, toJSON b)
        , (fieldName @c, toJSON c)
        , (fieldName @d, toJSON d)
        , (fieldName @e, toJSON e)
        ]

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON  a, FromJSON a,
    JsonFieldName b, ToJSON  b, FromJSON b,
    JsonFieldName c, ToJSON  c, FromJSON c,
    JsonFieldName d, ToJSON  d, FromJSON d,
    JsonFieldName e, ToJSON  e, FromJSON e,
    JsonFieldName f, ToJSON  f, FromJSON f
  ) =>
  ToJSON (NamedFields (a, b, c, d, e, f))
  where
    toJSON (NamedFields (a, b, c, d, e, f)) = Object
        [ (fieldName @a, toJSON a)
        , (fieldName @b, toJSON b)
        , (fieldName @c, toJSON c)
        , (fieldName @d, toJSON d)
        , (fieldName @e, toJSON e)
        , (fieldName @f, toJSON f)
        ]

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON  a, FromJSON a,
    JsonFieldName b, ToJSON  b, FromJSON b,
    JsonFieldName c, ToJSON  c, FromJSON c,
    JsonFieldName d, ToJSON  d, FromJSON d,
    JsonFieldName e, ToJSON  e, FromJSON e,
    JsonFieldName f, ToJSON  f, FromJSON f,
    JsonFieldName g, ToJSON  g, FromJSON g
  ) =>
  ToJSON (NamedFields (a, b, c, d, e, f, g))
  where
    toJSON (NamedFields (a, b, c, d, e, f, g)) = Object
        [ (fieldName @a, toJSON a)
        , (fieldName @b, toJSON b)
        , (fieldName @c, toJSON c)
        , (fieldName @d, toJSON d)
        , (fieldName @e, toJSON e)
        , (fieldName @f, toJSON f)
        , (fieldName @g, toJSON g)
        ]

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON  a, FromJSON a,
    JsonFieldName b, ToJSON  b, FromJSON b,
    JsonFieldName c, ToJSON  c, FromJSON c,
    JsonFieldName d, ToJSON  d, FromJSON d,
    JsonFieldName e, ToJSON  e, FromJSON e,
    JsonFieldName f, ToJSON  f, FromJSON f,
    JsonFieldName g, ToJSON  g, FromJSON g,
    JsonFieldName h, ToJSON  h, FromJSON h
  ) =>
  ToJSON (NamedFields (a, b, c, d, e, f, g, h))
  where
    toJSON (NamedFields (a, b, c, d, e, f, g, h)) = Object
        [ (fieldName @a, toJSON a)
        , (fieldName @b, toJSON b)
        , (fieldName @c, toJSON c)
        , (fieldName @d, toJSON d)
        , (fieldName @e, toJSON e)
        , (fieldName @f, toJSON f)
        , (fieldName @g, toJSON g)
        , (fieldName @h, toJSON h)
        ]

------------------------------------------------------------------------------------------
instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON  a, FromJSON a,
    JsonFieldName b, ToJSON  b, FromJSON b
  ) =>
  FromJSON (NamedFields (a, b))
  where
    parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
        (,) <$> v .: fieldName @a <*> v .: fieldName @b

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON  a, FromJSON a,
    JsonFieldName b, ToJSON  b, FromJSON b,
    JsonFieldName c, ToJSON  c, FromJSON c
  ) =>
  FromJSON (NamedFields (a, b, c))
  where
    parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
        (,,) <$> v .: fieldName @a <*> v .: fieldName @b <*> v .: fieldName @c

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON  a, FromJSON a,
    JsonFieldName b, ToJSON  b, FromJSON b,
    JsonFieldName c, ToJSON  c, FromJSON c,
    JsonFieldName d, ToJSON  d, FromJSON d
  ) =>
  FromJSON (NamedFields (a, b, c, d))
  where
    parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
        (,,,)
            <$> v
            .:  fieldName @a
            <*> v
            .:  fieldName @b
            <*> v
            .:  fieldName @c
            <*> v
            .:  fieldName @d

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON  a, FromJSON a,
    JsonFieldName b, ToJSON  b, FromJSON b,
    JsonFieldName c, ToJSON  c, FromJSON c,
    JsonFieldName d, ToJSON  d, FromJSON d,
    JsonFieldName e, ToJSON  e, FromJSON e
  ) =>
  FromJSON (NamedFields (a, b, c, d, e))
  where
    parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
        (,,,,)
            <$> v
            .:  fieldName @a
            <*> v
            .:  fieldName @b
            <*> v
            .:  fieldName @c
            <*> v
            .:  fieldName @d
            <*> v
            .:  fieldName @e

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON  a, FromJSON a,
    JsonFieldName b, ToJSON  b, FromJSON b,
    JsonFieldName c, ToJSON  c, FromJSON c,
    JsonFieldName d, ToJSON  d, FromJSON d,
    JsonFieldName e, ToJSON  e, FromJSON e,
    JsonFieldName f, ToJSON  f, FromJSON f
  ) =>
  FromJSON (NamedFields (a, b, c, d, e, f))
  where
    parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
        (,,,,,)
            <$> v
            .:  fieldName @a
            <*> v
            .:  fieldName @b
            <*> v
            .:  fieldName @c
            <*> v
            .:  fieldName @d
            <*> v
            .:  fieldName @e
            <*> v
            .:  fieldName @f

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON  a, FromJSON a,
    JsonFieldName b, ToJSON  b, FromJSON b,
    JsonFieldName c, ToJSON  c, FromJSON c,
    JsonFieldName d, ToJSON  d, FromJSON d,
    JsonFieldName e, ToJSON  e, FromJSON e,
    JsonFieldName f, ToJSON  f, FromJSON f,
    JsonFieldName g, ToJSON  g, FromJSON g
  ) =>
  FromJSON (NamedFields (a, b, c, d, e, f, g))
  where
    parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
        (,,,,,,)
            <$> v
            .:  fieldName @a
            <*> v
            .:  fieldName @b
            <*> v
            .:  fieldName @c
            <*> v
            .:  fieldName @d
            <*> v
            .:  fieldName @e
            <*> v
            .:  fieldName @f
            <*> v
            .:  fieldName @g

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a, ToJSON  a, FromJSON a,
    JsonFieldName b, ToJSON  b, FromJSON b,
    JsonFieldName c, ToJSON  c, FromJSON c,
    JsonFieldName d, ToJSON  d, FromJSON d,
    JsonFieldName e, ToJSON  e, FromJSON e,
    JsonFieldName f, ToJSON  f, FromJSON f,
    JsonFieldName g, ToJSON  g, FromJSON g,
    JsonFieldName h, ToJSON  h, FromJSON h
  ) =>
  FromJSON (NamedFields (a, b, c, d, e, f, g, h))
  where
    parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
        (,,,,,,,)
            <$> v
            .:  fieldName @a
            <*> v
            .:  fieldName @b
            <*> v
            .:  fieldName @c
            <*> v
            .:  fieldName @d
            <*> v
            .:  fieldName @e
            <*> v
            .:  fieldName @f
            <*> v
            .:  fieldName @g
            <*> v
            .:  fieldName @h


--
--
--
--
------------------------------------------------------------------------------------------
-- Funcprog discussion
------------------------------------------------------------------------------------------
-- Tommy Engström Today at 10:00 AM
-- I'm trying to write a set of tuples Fancy1 ... FancyN. The fancy is that the To/FromJSON instances require each parameter to implement FieldName, which just tells you what name the field should have in the object.
-- This works fine and all, but what would be sweet is if I could get a type error if the names would overlap. How can I go about implementing this?
--
-- TheMatten:zulip:  1 hour ago
-- FieldName is class?
--
-- Tommy Engström  1 hour ago
-- yes
--
-- Tommy Engström  1 hour ago
-- now it contains fieldName :: Proxy a -> Text, but it can be changed.
--
-- TheMatten:zulip:  1 hour ago
-- And FieldName is uniquely determined by type of field?
--
-- Tommy Engström  1 hour ago
-- yes, that is the idea
--
-- Tommy Engström  1 hour ago
-- but types could define same name. would  not be a big deal if we didn't pick that up though.
--
-- TheMatten:zulip:  1 hour ago
-- I would probably do
-- type family FieldName a = (s :: Symbol) | s -> a
-- then - it forces types to have unique names
-- Then you could simply derive via newtype, instance of which checks Generic ally that same field type isn't used twice (edited)
--
-- Tommy Engström  1 hour ago
-- But the main issue is that I want to disallow someone from creating Fancy2 Int Int, or any other fancy tuple container more than one of a specific type.
--
-- Tommy Engström  1 hour ago
-- the idea is that encode $ Fancy2 (1 :: Int) (2 :: Double) should give you:
-- {int: 1
-- , double: 2.0
-- }
--
-- Tommy Engström  1 hour ago
-- I mean, this is intended to be used with a Wrapped (s :: Symbol) a type, so the names should make more sense.
--
-- TheMatten:zulip:  1 hour ago
-- Yeah - you would make some
-- newtype Distinct a = Distinct a
-- instance DistinctFields a => ToJSON (Distinct a) where ...
-- class (Generic a, DistinctFieldsG (Rep a)) => DistinctFields a
-- instance (Generic a, DistinctFieldsG (Rep a)) => DistinctFields a
-- where DistinctFieldsG could have some accumulator and check that encountered fields do no appear in it already, or type family that returns unique subfields or fails
--
-- Tommy Engström  1 hour ago
-- Thanks. I'll give it a try.
-- :+1:
-- 1
--
--
-- TheMatten:zulip:  1 hour ago
-- BTW, TH is always an option :smile:
--
-- Tommy Engström  40 minutes ago
-- True, I considered it as this is intended to be used in a TH thingie. But then the check is really outside of the instance, which feels wierd. At least in the solution I came up with, you may have a better one.
--
-- TheMatten:zulip:  39 minutes ago
-- You could wrap aeson 's TH in function that does that check to ensure they're coupled
--
-- Tommy Engström  37 minutes ago
-- I don't understand. But I'll give the generics approach a shot
--
-- TheMatten:zulip:  36 minutes ago
-- Like deriveDistinctFieldsJSON that only returns derived instance from deriveJSON if it's fields are distinct
--
-- Tommy Engström  33 minutes ago
-- Hmm, ok. I'll try it if I fail with the first attempt. :)
-- :+1:
-- 1
