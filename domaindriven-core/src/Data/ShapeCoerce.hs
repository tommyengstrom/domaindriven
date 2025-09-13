{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-operator-whitespace #-}

module Data.ShapeCoerce where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import GHC.Generics
import GHC.TypeLits
import Prelude

gshapeCoerce
    :: forall a b. (Generic a, Generic b, GShapeCoercible a b (Rep a) (Rep b)) => a -> b
gshapeCoerce = to . gscoerce @a @b . from

-- Though intuitively two types are coercible iff they are isomorphic let's be
-- explicit about it.
type ShapeIsomorphic a b = (ShapeCoercible a b, ShapeCoercible b a)

class ShapeCoercible a b where
    shapeCoerce :: a -> b

instance
    {-# OVERLAPPABLE #-}
    (Generic a, Generic b, GShapeCoercible a b (Rep a) (Rep b))
    => ShapeCoercible a b
    where
    shapeCoerce = to . gscoerce @a @b . from

instance (ShapeCoercible a c, ShapeCoercible b d, Ord c) => ShapeCoercible (Map a b) (Map c d) where
    shapeCoerce = M.mapKeys shapeCoerce . M.map shapeCoerce

instance {-# OVERLAPPING #-} ShapeCoercible a a where
    shapeCoerce = id

class GShapeCoercible x y a b where
    gscoerce :: a x -> b x

instance
    forall x y f1 f2 datatypeName a1 b1 c1 a2 b2 c2
     . GShapeCoercible x y f1 f2
    => GShapeCoercible
        x
        y
        (M1 D ('MetaData datatypeName a1 b1 c1) f1)
        (M1 D ('MetaData datatypeName a2 b2 c2) f2)
    where
    gscoerce = M1 . gscoerce @x @y . unM1

-- Better error for incompatible data types
instance
    {-# OVERLAPPABLE #-}
    forall x y f1 f2 datatypeName1 datatypeName2 a1 b1 c1 a2 b2 c2
     . TypeError
        ( 'Text "Cannot shapeCoerce between types:"
            ':$$: 'Text "  From: " ':<>: 'ShowType x
            ':$$: 'Text "  To: " ':<>: 'ShowType y
            ':$$: 'Text ""
            ':$$: 'Text "Reason: Incompatible data types"
            ':$$: 'Text "  From type: " ':<>: 'ShowType datatypeName1
            ':$$: 'Text "  To type: " ':<>: 'ShowType datatypeName2
            ':$$: 'Text ""
            ':$$: 'Text "Solution: Write instance `ShapeCoercible "
                ':<>: 'ShowType x
                ':<>: 'Text " "
                ':<>: 'ShowType y
                ':<>: 'Text "`"
        )
    => GShapeCoercible
        x
        y
        (M1 D ('MetaData datatypeName1 a1 b1 c1) f1)
        (M1 D ('MetaData datatypeName2 a2 b2 c2) f2)
    where
    gscoerce = error "unreachable"

-- Matching constructor names with same structure
instance
    GShapeCoercible x y f1 f2
    => GShapeCoercible
        x
        y
        (M1 C ('MetaCons constructorName b c) f1)
        (M1 C ('MetaCons constructorName b c) f2)
    where
    gscoerce = M1 . gscoerce @x @y . unM1

-- Same constructor name but different structure
instance
    {-# OVERLAPPABLE #-}
    forall x y f1 f2 name b1 c1 b2 c2
     . TypeError
        ( 'Text "Cannot shapeCoerce between types:"
            ':$$: 'Text "  From: " ':<>: 'ShowType x
            ':$$: 'Text "  To: " ':<>: 'ShowType y
            ':$$: 'Text ""
            ':$$: 'Text "Reason: Constructor "
                ':<>: 'ShowType name
                ':<>: 'Text " has different field structures"
            ':$$: 'Text ""
            ':$$: 'Text "Solution: Write instance `ShapeCoercible "
                ':<>: 'ShowType x
                ':<>: 'Text " "
                ':<>: 'ShowType y
                ':<>: 'Text "`"
        )
    => GShapeCoercible
        x
        y
        (M1 C ('MetaCons name b1 c1) f1)
        (M1 C ('MetaCons name b2 c2) f2)
    where
    gscoerce = error "unreachable"

-- Different constructor names
instance
    {-# OVERLAPPABLE #-}
    forall x y f1 f2 cName1 cName2 b1 c1 b2 c2
     . TypeError
        ( 'Text "Cannot shapeCoerce between types:"
            ':$$: 'Text "  From: " ':<>: 'ShowType x
            ':$$: 'Text "  To: " ':<>: 'ShowType y
            ':$$: 'Text ""
            ':$$: 'Text "Reason: Constructor name mismatch"
            ':$$: 'Text "  " ':<>: 'ShowType cName1 ':<>: 'Text " ≠ " ':<>: 'ShowType cName2
            ':$$: 'Text ""
            ':$$: 'Text "Solution: Write instance `ShapeCoercible "
                ':<>: 'ShowType x
                ':<>: 'Text " "
                ':<>: 'ShowType y
                ':<>: 'Text "`"
        )
    => GShapeCoercible
        x
        y
        (M1 C ('MetaCons cName1 b1 c1) f1)
        (M1 C ('MetaCons cName2 b2 c2) f2)
    where
    gscoerce = error "unreachable"

instance
    GShapeCoercible x y f1 f2
    => GShapeCoercible
        x
        y
        (M1 S ('MetaSel selectorName a1 b1 c1) f1)
        (M1 S ('MetaSel selectorName a2 b2 c2) f2)
    where
    gscoerce = M1 . gscoerce @x @y . unM1

-- Instance for mismatched selector names
instance
    {-# OVERLAPPABLE #-}
    forall x y f1 f2 name1 name2 a1 b1 c1 a2 b2 c2
     . TypeError
        ( 'Text "Cannot shapeCoerce between types:"
            ':$$: 'Text "  From: " ':<>: 'ShowType x
            ':$$: 'Text "  To: " ':<>: 'ShowType y
            ':$$: 'Text ""
            ':$$: 'Text "Reason: Field name mismatch"
            ':$$: 'Text "  Expected: " ':<>: 'ShowType name1
            ':$$: 'Text "  But got: " ':<>: 'ShowType name2
            ':$$: 'Text ""
            ':$$: 'Text "Solution: Write instance `ShapeCoercible "
                ':<>: 'ShowType x
                ':<>: 'Text " "
                ':<>: 'ShowType y
                ':<>: 'Text "`"
        )
    => GShapeCoercible
        x
        y
        (M1 S ('MetaSel name1 a1 b1 c1) f1)
        (M1 S ('MetaSel name2 a2 b2 c2) f2)
    where
    gscoerce = error "unreachable"

instance
    (GShapeCoercible x y a1 a2, GShapeCoercible x y b1 b2)
    => GShapeCoercible x y (a1 :*: b1) (a2 :*: b2)
    where
    gscoerce (a :*: b) = gscoerce @x @y a :*: gscoerce @x @y b

instance
    (GShapeCoercible x y a1 a2, GShapeCoercible x y b1 b2)
    => GShapeCoercible x y (a1 :+: b1) (a2 :+: b2)
    where
    gscoerce (L1 a) = L1 $ gscoerce @x @y a
    gscoerce (R1 b) = R1 $ gscoerce @x @y b

-- Single constructor vs sum type (left to right)
instance
    {-# OVERLAPPABLE #-}
    forall x y c name b p f rest
     . TypeError
        ( 'Text "Cannot shapeCoerce between types:"
            ':$$: 'Text "  From: " ':<>: 'ShowType x
            ':$$: 'Text "  To: " ':<>: 'ShowType y
            ':$$: 'Text ""
            ':$$: 'Text "Reason: Left side has a single constructor but right side is a sum type"
            ':$$: 'Text "Left constructor: " ':<>: 'ShowType name
            ':$$: 'Text "Right side: Multiple constructors (sum type)"
            ':$$: 'Text ""
            ':$$: 'Text "Solution: Write instance `ShapeCoercible "
                ':<>: 'ShowType x
                ':<>: 'Text " "
                ':<>: 'ShowType y
                ':<>: 'Text "`"
        )
    => GShapeCoercible x y (M1 C ('MetaCons name b p) f) (c :+: rest)
    where
    gscoerce = error "unreachable"

-- Sum type vs single constructor (right to left)
instance
    {-# OVERLAPPABLE #-}
    forall x y c name b p f rest
     . TypeError
        ( 'Text "Cannot shapeCoerce between types:"
            ':$$: 'Text "  From: " ':<>: 'ShowType x
            ':$$: 'Text "  To: " ':<>: 'ShowType y
            ':$$: 'Text ""
            ':$$: 'Text "Reason: Left side is a sum type but right side has a single constructor"
            ':$$: 'Text "Right constructor: " ':<>: 'ShowType name
            ':$$: 'Text ""
            ':$$: 'Text "Solution: Write instance `ShapeCoercible "
                ':<>: 'ShowType x
                ':<>: 'Text " "
                ':<>: 'ShowType y
                ':<>: 'Text "`"
        )
    => GShapeCoercible x y (c :+: rest) (M1 C ('MetaCons name b p) f)
    where
    gscoerce = error "unreachable"

instance GShapeCoercible x y U1 U1 where
    gscoerce = id

-- Better error for U1 vs field mismatch
instance
    {-# OVERLAPPABLE #-}
    forall x y name a b c t
     . TypeError
        ( 'Text "Cannot shapeCoerce between types:"
            ':$$: 'Text "  From: " ':<>: 'ShowType x
            ':$$: 'Text "  To: " ':<>: 'ShowType y
            ':$$: 'Text ""
            ':$$: 'Text "Reason: Constructor has no fields but expected field: " ':<>: 'ShowType name
            ':$$: 'Text ""
            ':$$: 'Text "Solution: Write instance `ShapeCoercible "
                ':<>: 'ShowType x
                ':<>: 'Text " "
                ':<>: 'ShowType y
                ':<>: 'Text "`"
        )
    => GShapeCoercible x y U1 (M1 S ('MetaSel name a b c) t)
    where
    gscoerce = error "unreachable"

-- Better error for field vs U1 mismatch
instance
    {-# OVERLAPPABLE #-}
    forall x y name a b c t
     . TypeError
        ( 'Text "Cannot shapeCoerce between types:"
            ':$$: 'Text "  From: " ':<>: 'ShowType x
            ':$$: 'Text "  To: " ':<>: 'ShowType y
            ':$$: 'Text ""
            ':$$: 'Text "Reason: Constructor has field "
                ':<>: 'ShowType name
                ':<>: 'Text " but none expected"
            ':$$: 'Text ""
            ':$$: 'Text "Solution: Write instance `ShapeCoercible "
                ':<>: 'ShowType x
                ':<>: 'Text " "
                ':<>: 'ShowType y
                ':<>: 'Text "`"
        )
    => GShapeCoercible x y (M1 S ('MetaSel name a b c) t) U1
    where
    gscoerce = error "unreachable"

-- Instance for U1 vs product (fields)
instance
    {-# OVERLAPPABLE #-}
    forall x y a b
     . TypeError
        ( 'Text "Cannot shapeCoerce between types:"
            ':$$: 'Text "  From: " ':<>: 'ShowType x
            ':$$: 'Text "  To: " ':<>: 'ShowType y
            ':$$: 'Text ""
            ':$$: 'Text "Reason: Constructor has no fields but expected multiple fields"
            ':$$: 'Text ""
            ':$$: 'Text "Solution: Write instance `ShapeCoercible "
                ':<>: 'ShowType x
                ':<>: 'Text " "
                ':<>: 'ShowType y
                ':<>: 'Text "`"
        )
    => GShapeCoercible x y U1 (a :*: b)
    where
    gscoerce = error "unreachable"

-- Instance for product vs U1
instance
    {-# OVERLAPPABLE #-}
    forall x y a b
     . TypeError
        ( 'Text "Cannot shapeCoerce between types:"
            ':$$: 'Text "  From: " ':<>: 'ShowType x
            ':$$: 'Text "  To: " ':<>: 'ShowType y
            ':$$: 'Text ""
            ':$$: 'Text "Reason: Constructor has fields but none expected"
            ':$$: 'Text ""
            ':$$: 'Text "Solution: Write instance `ShapeCoercible "
                ':<>: 'ShowType x
                ':<>: 'Text " "
                ':<>: 'ShowType y
                ':<>: 'Text "`"
        )
    => GShapeCoercible x y (a :*: b) U1
    where
    gscoerce = error "unreachable"

-- Instance for single field vs product (multiple fields)
instance
    {-# OVERLAPPABLE #-}
    forall x y s meta f rest
     . TypeError
        ( 'Text "Cannot shapeCoerce between types:"
            ':$$: 'Text "  From: " ':<>: 'ShowType x
            ':$$: 'Text "  To: " ':<>: 'ShowType y
            ':$$: 'Text ""
            ':$$: 'Text "Reason: Field count mismatch (single field vs multiple fields)"
            ':$$: 'Text ""
            ':$$: 'Text "Solution: Write instance `ShapeCoercible "
                ':<>: 'ShowType x
                ':<>: 'Text " "
                ':<>: 'ShowType y
                ':<>: 'Text "`"
        )
    => GShapeCoercible x y (M1 S meta f) (s :*: rest)
    where
    gscoerce = error "unreachable"

-- Instance for product vs single field
instance
    {-# OVERLAPPABLE #-}
    forall x y s meta f rest
     . TypeError
        ( 'Text "Cannot shapeCoerce between types:"
            ':$$: 'Text "  From: " ':<>: 'ShowType x
            ':$$: 'Text "  To: " ':<>: 'ShowType y
            ':$$: 'Text ""
            ':$$: 'Text "Reason: Field count mismatch (multiple fields vs single field)"
            ':$$: 'Text ""
            ':$$: 'Text "Solution: Write instance `ShapeCoercible "
                ':<>: 'ShowType x
                ':<>: 'Text " "
                ':<>: 'ShowType y
                ':<>: 'Text "`"
        )
    => GShapeCoercible x y (s :*: rest) (M1 S meta f)
    where
    gscoerce = error "unreachable"

instance GShapeCoercible x y (M1 S s (Rec0 ())) U1 where
    gscoerce _ = U1

instance
    ShapeCoercible c1 c2
    => GShapeCoercible x y (Rec0 c1) (Rec0 c2)
    where
    gscoerce (K1 x) = K1 $ shapeCoerce x
