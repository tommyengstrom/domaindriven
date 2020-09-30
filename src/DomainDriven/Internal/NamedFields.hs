{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.Internal.NamedFields where

import Control.Lens
  ( (.~),
    (?~),
  )
import Data.Aeson
import Data.Swagger
import DomainDriven.Internal.JsonFieldName
import RIO

data NamedFields a = NamedFields {unNamedFields :: a}
  deriving (Show, Eq, Ord)

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b
  ) =>
  ToJSON (NamedFields (a, b))
  where
  toJSON (NamedFields (a, b)) =
    Object [(fieldName @a, toJSON a), (fieldName @b, toJSON b)]

instance (JsonFieldName a, JsonFieldName b) => ToSchema (NamedFields (a, b)) where
  declareNamedSchema _ = do
    a <- declareSchemaRef $ Proxy @a
    b <- declareSchemaRef $ Proxy @b
    pure $
      NamedSchema Nothing $
        mempty
          & type_
          ?~ SwaggerObject
          & properties
          .~ [(fieldName @a, a), (fieldName @b, b)]

instance
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c
  ) =>
  ToSchema (NamedFields (a, b, c))
  where
  declareNamedSchema _ = do
    a <- declareSchemaRef $ Proxy @a
    b <- declareSchemaRef $ Proxy @b
    c <- declareSchemaRef $ Proxy @c
    pure $
      NamedSchema Nothing $
        mempty
          & type_
          ?~ SwaggerObject
          & properties
          .~ [ (fieldName @a, a),
               (fieldName @b, b),
               (fieldName @c, c)
             ]

instance
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d
  ) =>
  ToSchema (NamedFields (a, b, c, d))
  where
  declareNamedSchema _ = do
    a <- declareSchemaRef $ Proxy @a
    b <- declareSchemaRef $ Proxy @b
    c <- declareSchemaRef $ Proxy @c
    d <- declareSchemaRef $ Proxy @d
    pure $
      NamedSchema Nothing $
        mempty
          & type_
          ?~ SwaggerObject
          & properties
          .~ [ (fieldName @a, a),
               (fieldName @b, b),
               (fieldName @c, c),
               (fieldName @d, d)
             ]

instance
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d,
    JsonFieldName e
  ) =>
  ToSchema (NamedFields (a, b, c, d, e))
  where
  declareNamedSchema _ = do
    a <- declareSchemaRef $ Proxy @a
    b <- declareSchemaRef $ Proxy @b
    c <- declareSchemaRef $ Proxy @c
    d <- declareSchemaRef $ Proxy @d
    e <- declareSchemaRef $ Proxy @e
    pure $
      NamedSchema Nothing $
        mempty
          & type_
          ?~ SwaggerObject
          & properties
          .~ [ (fieldName @a, a),
               (fieldName @b, b),
               (fieldName @c, c),
               (fieldName @d, d),
               (fieldName @e, e)
             ]

instance
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d,
    JsonFieldName e,
    JsonFieldName f
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
    pure $
      NamedSchema Nothing $
        mempty
          & type_
          ?~ SwaggerObject
          & properties
          .~ [ (fieldName @a, a),
               (fieldName @b, b),
               (fieldName @c, c),
               (fieldName @d, d),
               (fieldName @e, e),
               (fieldName @f, f)
             ]

instance
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d,
    JsonFieldName e,
    JsonFieldName f,
    JsonFieldName g
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
    pure $
      NamedSchema Nothing $
        mempty
          & type_
          ?~ SwaggerObject
          & properties
          .~ [ (fieldName @a, a),
               (fieldName @b, b),
               (fieldName @c, c),
               (fieldName @d, d),
               (fieldName @e, e),
               (fieldName @f, f),
               (fieldName @g, g)
             ]

instance
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d,
    JsonFieldName e,
    JsonFieldName f,
    JsonFieldName g,
    JsonFieldName h
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
    pure $
      NamedSchema Nothing $
        mempty
          & type_
          ?~ SwaggerObject
          & properties
          .~ [ (fieldName @a, a),
               (fieldName @b, b),
               (fieldName @c, c),
               (fieldName @d, d),
               (fieldName @e, e),
               (fieldName @f, f),
               (fieldName @g, g),
               (fieldName @h, h)
             ]

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c
  ) =>
  ToJSON (NamedFields (a, b, c))
  where
  toJSON (NamedFields (a, b, c)) =
    Object
      [ (fieldName @a, toJSON a),
        (fieldName @b, toJSON b),
        (fieldName @c, toJSON c)
      ]

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d
  ) =>
  ToJSON (NamedFields (a, b, c, d))
  where
  toJSON (NamedFields (a, b, c, d)) =
    Object
      [ (fieldName @a, toJSON a),
        (fieldName @b, toJSON b),
        (fieldName @c, toJSON c),
        (fieldName @d, toJSON d)
      ]

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d,
    JsonFieldName e
  ) =>
  ToJSON (NamedFields (a, b, c, d, e))
  where
  toJSON (NamedFields (a, b, c, d, e)) =
    Object
      [ (fieldName @a, toJSON a),
        (fieldName @b, toJSON b),
        (fieldName @c, toJSON c),
        (fieldName @d, toJSON d),
        (fieldName @e, toJSON e)
      ]

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d,
    JsonFieldName e,
    JsonFieldName f
  ) =>
  ToJSON (NamedFields (a, b, c, d, e, f))
  where
  toJSON (NamedFields (a, b, c, d, e, f)) =
    Object
      [ (fieldName @a, toJSON a),
        (fieldName @b, toJSON b),
        (fieldName @c, toJSON c),
        (fieldName @d, toJSON d),
        (fieldName @e, toJSON e),
        (fieldName @f, toJSON f)
      ]

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d,
    JsonFieldName e,
    JsonFieldName f,
    JsonFieldName g
  ) =>
  ToJSON (NamedFields (a, b, c, d, e, f, g))
  where
  toJSON (NamedFields (a, b, c, d, e, f, g)) =
    Object
      [ (fieldName @a, toJSON a),
        (fieldName @b, toJSON b),
        (fieldName @c, toJSON c),
        (fieldName @d, toJSON d),
        (fieldName @e, toJSON e),
        (fieldName @f, toJSON f),
        (fieldName @g, toJSON g)
      ]

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d,
    JsonFieldName e,
    JsonFieldName f,
    JsonFieldName g,
    JsonFieldName h
  ) =>
  ToJSON (NamedFields (a, b, c, d, e, f, g, h))
  where
  toJSON (NamedFields (a, b, c, d, e, f, g, h)) =
    Object
      [ (fieldName @a, toJSON a),
        (fieldName @b, toJSON b),
        (fieldName @c, toJSON c),
        (fieldName @d, toJSON d),
        (fieldName @e, toJSON e),
        (fieldName @f, toJSON f),
        (fieldName @g, toJSON g),
        (fieldName @h, toJSON h)
      ]

instance ToJSON a => ToJSON (NamedFields a) where
  toJSON (NamedFields a) = toJSON a

------------------------------------------------------------------------------------------
instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b
  ) =>
  FromJSON (NamedFields (a, b))
  where
  parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
    (,) <$> v .: fieldName @a <*> v .: fieldName @b

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c
  ) =>
  FromJSON (NamedFields (a, b, c))
  where
  parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
    (,,)
      <$> v
      .: fieldName @a
      <*> v
      .: fieldName @b
      <*> v
      .: fieldName @c

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d
  ) =>
  FromJSON (NamedFields (a, b, c, d))
  where
  parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
    (,,,)
      <$> v
      .: fieldName @a
      <*> v
      .: fieldName @b
      <*> v
      .: fieldName @c
      <*> v
      .: fieldName @d

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d,
    JsonFieldName e
  ) =>
  FromJSON (NamedFields (a, b, c, d, e))
  where
  parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
    (,,,,)
      <$> v
      .: fieldName @a
      <*> v
      .: fieldName @b
      <*> v
      .: fieldName @c
      <*> v
      .: fieldName @d
      <*> v
      .: fieldName @e

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d,
    JsonFieldName e,
    JsonFieldName f
  ) =>
  FromJSON (NamedFields (a, b, c, d, e, f))
  where
  parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
    (,,,,,)
      <$> v
      .: fieldName @a
      <*> v
      .: fieldName @b
      <*> v
      .: fieldName @c
      <*> v
      .: fieldName @d
      <*> v
      .: fieldName @e
      <*> v
      .: fieldName @f

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d,
    JsonFieldName e,
    JsonFieldName f,
    JsonFieldName g
  ) =>
  FromJSON (NamedFields (a, b, c, d, e, f, g))
  where
  parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
    (,,,,,,)
      <$> v
      .: fieldName @a
      <*> v
      .: fieldName @b
      <*> v
      .: fieldName @c
      <*> v
      .: fieldName @d
      <*> v
      .: fieldName @e
      <*> v
      .: fieldName @f
      <*> v
      .: fieldName @g

instance
  {-# OVERLAPPING #-}
  ( JsonFieldName a,
    JsonFieldName b,
    JsonFieldName c,
    JsonFieldName d,
    JsonFieldName e,
    JsonFieldName f,
    JsonFieldName g,
    JsonFieldName h
  ) =>
  FromJSON (NamedFields (a, b, c, d, e, f, g, h))
  where
  parseJSON = fmap (fmap NamedFields) . withObject ("NamedFields") $ \v ->
    (,,,,,,,)
      <$> v
      .: fieldName @a
      <*> v
      .: fieldName @b
      <*> v
      .: fieldName @c
      <*> v
      .: fieldName @d
      <*> v
      .: fieldName @e
      <*> v
      .: fieldName @f
      <*> v
      .: fieldName @g
      <*> v
      .: fieldName @h

instance FromJSON a => FromJSON (NamedFields a) where
  parseJSON = fmap NamedFields . parseJSON

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
