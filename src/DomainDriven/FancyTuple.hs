{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.FancyTuple where

import Data.Aeson
import RIO

class (ToJSON t, FromJSON t) => JSONFieldName t where
  fieldName :: Proxy t -> Text

instance JSONFieldName Int where
  fieldName _ = "int"

instance JSONFieldName Double where
  fieldName _ = "double"

instance JSONFieldName Text where
  fieldName _ = "text"

instance JSONFieldName String where
  fieldName _ = "string"

instance JSONFieldName Bool where
  fieldName _ = "bool"

data Fancy a = Fancy {unFancy :: a}
  deriving (Show, Eq, Ord)

instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b
  ) =>
  ToJSON (Fancy (a, b))
  where
  toJSON (Fancy (a, b)) =
    Object
      [ (fieldName (Proxy @a), toJSON a),
        (fieldName (Proxy @b), toJSON b)
      ]

instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b,
    JSONFieldName c
  ) =>
  ToJSON (Fancy (a, b, c))
  where
  toJSON (Fancy (a, b, c)) =
    Object
      [ (fieldName (Proxy @a), toJSON a),
        (fieldName (Proxy @b), toJSON b),
        (fieldName (Proxy @c), toJSON c)
      ]

instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b,
    JSONFieldName c,
    JSONFieldName d
  ) =>
  ToJSON (Fancy (a, b, c, d))
  where
  toJSON (Fancy (a, b, c, d)) =
    Object
      [ (fieldName (Proxy @a), toJSON a),
        (fieldName (Proxy @b), toJSON b),
        (fieldName (Proxy @c), toJSON c),
        (fieldName (Proxy @d), toJSON d)
      ]

instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b,
    JSONFieldName c,
    JSONFieldName d,
    JSONFieldName e
  ) =>
  ToJSON (Fancy (a, b, c, d, e))
  where
  toJSON (Fancy (a, b, c, d, e)) =
    Object
      [ (fieldName (Proxy @a), toJSON a),
        (fieldName (Proxy @b), toJSON b),
        (fieldName (Proxy @c), toJSON c),
        (fieldName (Proxy @d), toJSON d),
        (fieldName (Proxy @e), toJSON e)
      ]

instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b,
    JSONFieldName c,
    JSONFieldName d,
    JSONFieldName e,
    JSONFieldName f
  ) =>
  ToJSON (Fancy (a, b, c, d, e, f))
  where
  toJSON (Fancy (a, b, c, d, e, f)) =
    Object
      [ (fieldName (Proxy @a), toJSON a),
        (fieldName (Proxy @b), toJSON b),
        (fieldName (Proxy @c), toJSON c),
        (fieldName (Proxy @d), toJSON d),
        (fieldName (Proxy @e), toJSON e),
        (fieldName (Proxy @f), toJSON f)
      ]

instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b,
    JSONFieldName c,
    JSONFieldName d,
    JSONFieldName e,
    JSONFieldName f,
    JSONFieldName g
  ) =>
  ToJSON (Fancy (a, b, c, d, e, f, g))
  where
  toJSON (Fancy (a, b, c, d, e, f, g)) =
    Object
      [ (fieldName (Proxy @a), toJSON a),
        (fieldName (Proxy @b), toJSON b),
        (fieldName (Proxy @c), toJSON c),
        (fieldName (Proxy @d), toJSON d),
        (fieldName (Proxy @e), toJSON e),
        (fieldName (Proxy @f), toJSON f),
        (fieldName (Proxy @g), toJSON g)
      ]

instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b,
    JSONFieldName c,
    JSONFieldName d,
    JSONFieldName e,
    JSONFieldName f,
    JSONFieldName g,
    JSONFieldName h
  ) =>
  ToJSON (Fancy (a, b, c, d, e, f, g, h))
  where
  toJSON (Fancy (a, b, c, d, e, f, g, h)) =
    Object
      [ (fieldName (Proxy @a), toJSON a),
        (fieldName (Proxy @b), toJSON b),
        (fieldName (Proxy @c), toJSON c),
        (fieldName (Proxy @d), toJSON d),
        (fieldName (Proxy @e), toJSON e),
        (fieldName (Proxy @f), toJSON f),
        (fieldName (Proxy @g), toJSON g),
        (fieldName (Proxy @h), toJSON h)
      ]

instance ToJSON a => ToJSON (Fancy a) where
  toJSON (Fancy a) = toJSON a

------------------------------------------------------------------------------------------
instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b
  ) =>
  FromJSON (Fancy (a, b))
  where
  parseJSON = fmap (fmap Fancy) . withObject ("Fancy") $ \v ->
    (,)
      <$> v .: fieldName (Proxy @a)
      <*> v .: fieldName (Proxy @b)

instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b,
    JSONFieldName c
  ) =>
  FromJSON (Fancy (a, b, c))
  where
  parseJSON = fmap (fmap Fancy) . withObject ("Fancy") $ \v ->
    (,,)
      <$> v .: fieldName (Proxy @a)
      <*> v .: fieldName (Proxy @b)
      <*> v .: fieldName (Proxy @c)

instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b,
    JSONFieldName c,
    JSONFieldName d
  ) =>
  FromJSON (Fancy (a, b, c, d))
  where
  parseJSON = fmap (fmap Fancy) . withObject ("Fancy") $ \v ->
    (,,,)
      <$> v .: fieldName (Proxy @a)
      <*> v .: fieldName (Proxy @b)
      <*> v .: fieldName (Proxy @c)
      <*> v .: fieldName (Proxy @d)

instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b,
    JSONFieldName c,
    JSONFieldName d,
    JSONFieldName e
  ) =>
  FromJSON (Fancy (a, b, c, d, e))
  where
  parseJSON = fmap (fmap Fancy) . withObject ("Fancy") $ \v ->
    (,,,,)
      <$> v .: fieldName (Proxy @a)
      <*> v .: fieldName (Proxy @b)
      <*> v .: fieldName (Proxy @c)
      <*> v .: fieldName (Proxy @d)
      <*> v .: fieldName (Proxy @e)

instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b,
    JSONFieldName c,
    JSONFieldName d,
    JSONFieldName e,
    JSONFieldName f
  ) =>
  FromJSON (Fancy (a, b, c, d, e, f))
  where
  parseJSON = fmap (fmap Fancy) . withObject ("Fancy") $ \v ->
    (,,,,,)
      <$> v .: fieldName (Proxy @a)
      <*> v .: fieldName (Proxy @b)
      <*> v .: fieldName (Proxy @c)
      <*> v .: fieldName (Proxy @d)
      <*> v .: fieldName (Proxy @e)
      <*> v .: fieldName (Proxy @f)

instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b,
    JSONFieldName c,
    JSONFieldName d,
    JSONFieldName e,
    JSONFieldName f,
    JSONFieldName g
  ) =>
  FromJSON (Fancy (a, b, c, d, e, f, g))
  where
  parseJSON = fmap (fmap Fancy) . withObject ("Fancy") $ \v ->
    (,,,,,,)
      <$> v .: fieldName (Proxy @a)
      <*> v .: fieldName (Proxy @b)
      <*> v .: fieldName (Proxy @c)
      <*> v .: fieldName (Proxy @d)
      <*> v .: fieldName (Proxy @e)
      <*> v .: fieldName (Proxy @f)
      <*> v .: fieldName (Proxy @g)

instance
  {-# OVERLAPPING #-}
  ( JSONFieldName a,
    JSONFieldName b,
    JSONFieldName c,
    JSONFieldName d,
    JSONFieldName e,
    JSONFieldName f,
    JSONFieldName g,
    JSONFieldName h
  ) =>
  FromJSON (Fancy (a, b, c, d, e, f, g, h))
  where
  parseJSON = fmap (fmap Fancy) . withObject ("Fancy") $ \v ->
    (,,,,,,,)
      <$> v .: fieldName (Proxy @a)
      <*> v .: fieldName (Proxy @b)
      <*> v .: fieldName (Proxy @c)
      <*> v .: fieldName (Proxy @d)
      <*> v .: fieldName (Proxy @e)
      <*> v .: fieldName (Proxy @f)
      <*> v .: fieldName (Proxy @g)
      <*> v .: fieldName (Proxy @h)

--
--instance
--  {-# OVERLAPPING #-}
--  ( JSONFieldName a,
--    JSONFieldName b,
--    JSONFieldName c
--  ) =>
--  FromJSON (Fancy (a, b, c))
--  where
--  toJSON (Fancy (a, b, c)) =
--    Object
--      [ (fieldName (Proxy @a), toJSON a),
--        (fieldName (Proxy @b), toJSON b),
--        (fieldName (Proxy @c), toJSON c)
--      ]
--
--instance
--  {-# OVERLAPPING #-}
--  ( JSONFieldName a,
--    JSONFieldName b,
--    JSONFieldName c,
--    JSONFieldName d
--  ) =>
--  FromJSON (Fancy (a, b, c, d))
--  where
--  toJSON (Fancy (a, b, c, d)) =
--    Object
--      [ (fieldName (Proxy @a), toJSON a),
--        (fieldName (Proxy @b), toJSON b),
--        (fieldName (Proxy @c), toJSON c),
--        (fieldName (Proxy @d), toJSON d)
--      ]
--
--instance
--  {-# OVERLAPPING #-}
--  ( JSONFieldName a,
--    JSONFieldName b,
--    JSONFieldName c,
--    JSONFieldName d,
--    JSONFieldName e
--  ) =>
--  FromJSON (Fancy (a, b, c, d, e))
--  where
--  toJSON (Fancy (a, b, c, d, e)) =
--    Object
--      [ (fieldName (Proxy @a), toJSON a),
--        (fieldName (Proxy @b), toJSON b),
--        (fieldName (Proxy @c), toJSON c),
--        (fieldName (Proxy @d), toJSON d),
--        (fieldName (Proxy @e), toJSON e)
--      ]
--
--instance
--  {-# OVERLAPPING #-}
--  ( JSONFieldName a,
--    JSONFieldName b,
--    JSONFieldName c,
--    JSONFieldName d,
--    JSONFieldName e,
--    JSONFieldName f
--  ) =>
--  FromJSON (Fancy (a, b, c, d, e, f))
--  where
--  toJSON (Fancy (a, b, c, d, e, f)) =
--    Object
--      [ (fieldName (Proxy @a), toJSON a),
--        (fieldName (Proxy @b), toJSON b),
--        (fieldName (Proxy @c), toJSON c),
--        (fieldName (Proxy @d), toJSON d),
--        (fieldName (Proxy @e), toJSON e),
--        (fieldName (Proxy @f), toJSON f)
--      ]
--
--instance
--  {-# OVERLAPPING #-}
--  ( JSONFieldName a,
--    JSONFieldName b,
--    JSONFieldName c,
--    JSONFieldName d,
--    JSONFieldName e,
--    JSONFieldName f,
--    JSONFieldName g
--  ) =>
--  FromJSON (Fancy (a, b, c, d, e, f, g))
--  where
--  toJSON (Fancy (a, b, c, d, e, f, g)) =
--    Object
--      [ (fieldName (Proxy @a), toJSON a),
--        (fieldName (Proxy @b), toJSON b),
--        (fieldName (Proxy @c), toJSON c),
--        (fieldName (Proxy @d), toJSON d),
--        (fieldName (Proxy @e), toJSON e),
--        (fieldName (Proxy @f), toJSON f),
--        (fieldName (Proxy @g), toJSON g)
--      ]
--
--instance
--  {-# OVERLAPPING #-}
--  ( JSONFieldName a,
--    JSONFieldName b,
--    JSONFieldName c,
--    JSONFieldName d,
--    JSONFieldName e,
--    JSONFieldName f,
--    JSONFieldName g,
--    JSONFieldName h
--  ) =>
--  FromJSON (Fancy (a, b, c, d, e, f, g, h))
--  where
--  toJSON (Fancy (a, b, c, d, e, f, g, h)) =
--    Object
--      [ (fieldName (Proxy @a), toJSON a),
--        (fieldName (Proxy @b), toJSON b),
--        (fieldName (Proxy @c), toJSON c),
--        (fieldName (Proxy @d), toJSON d),
--        (fieldName (Proxy @e), toJSON e),
--        (fieldName (Proxy @f), toJSON f),
--        (fieldName (Proxy @g), toJSON g),
--        (fieldName (Proxy @h), toJSON h)
--      ]

instance FromJSON a => FromJSON (Fancy a) where
  parseJSON = fmap Fancy . parseJSON

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
