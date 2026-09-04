{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-operator-whitespace #-}

module Data.ShapeCoerce
    ( gshapeCoerce
    , ShapeIsomorphic
    , ShapeCoercible (..)
    , GShapeCoercible (..)
    )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import GHC.Generics
import GHC.TypeLits
import Prelude

gshapeCoerce
    :: forall a b. (Generic a, Generic b, GShapeCoercible a b (Rep a) (Rep b)) => a -> b
gshapeCoerce = to . gscoerce @a @b . from

-- Shape coercion is directional: a type can be coerced into a wider sum type
-- even though the reverse conversion is not possible. Isomorphism explicitly
-- requires coercion in both directions.
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

type family ConstructorCount (f :: Type -> Type) :: Nat where
    ConstructorCount (left :+: right) = ConstructorCount left + ConstructorCount right
    ConstructorCount (M1 C meta f) = 1

type family ConstructorNames (f :: Type -> Type) :: [Symbol] where
    ConstructorNames (left :+: right) = Append (ConstructorNames left) (ConstructorNames right)
    ConstructorNames (M1 C ('MetaCons name fixity record) f) = '[name]

type family Append (left :: [a]) (right :: [a]) :: [a] where
    Append '[] right = right
    Append (x ': xs) right = x ': Append xs right

type family IsOrderedSubsequence (source :: [Symbol]) (target :: [Symbol]) :: Bool where
    IsOrderedSubsequence '[] target = 'True
    IsOrderedSubsequence source '[] = 'False
    IsOrderedSubsequence (name ': source) (name ': target) = IsOrderedSubsequence source target
    IsOrderedSubsequence source (_ ': target) = IsOrderedSubsequence source target

type family Or (left :: Bool) (right :: Bool) :: Bool where
    Or 'True right = 'True
    Or 'False right = right

type family HasConstructor (name :: Symbol) (f :: Type -> Type) :: Bool where
    HasConstructor name (left :+: right) = Or (HasConstructor name left) (HasConstructor name right)
    HasConstructor name (M1 C ('MetaCons name fixity record) f) = 'True
    HasConstructor name (M1 C meta f) = 'False

class GShapeDatatype (countComparison :: Ordering) x y source target where
    gshapeDatatype :: source x -> target x

instance
    GShapeCoercible x y source target
    => GShapeDatatype 'EQ x y source target
    where
    gshapeDatatype = gscoerce @x @y

instance
    GShapeCoercible x y source target
    => GShapeDatatype 'GT x y source target
    where
    gshapeDatatype = gscoerce @x @y

instance
    GShapeWidening
        (IsOrderedSubsequence (ConstructorNames source) (ConstructorNames target))
        (ConstructorNames source)
        (ConstructorNames target)
        x
        y
        source
        target
    => GShapeDatatype 'LT x y source target
    where
    gshapeDatatype =
        gshapeWidening
            @(IsOrderedSubsequence (ConstructorNames source) (ConstructorNames target))
            @(ConstructorNames source)
            @(ConstructorNames target)
            @x
            @y

class
    GShapeWidening
        (ordered :: Bool)
        (sourceNames :: [Symbol])
        (targetNames :: [Symbol])
        x
        y
        source
        target
    where
    gshapeWidening :: source x -> target x

instance
    GWidenConstructors x y source target
    => GShapeWidening 'True sourceNames targetNames x y source target
    where
    gshapeWidening = gwidenConstructors @x @y

instance
    TypeError
        ( 'Text "Cannot shapeCoerce between types:"
            ':$$: 'Text "  From: " ':<>: 'ShowType x
            ':$$: 'Text "  To: " ':<>: 'ShowType y
            ':$$: 'Text ""
            ':$$: 'Text "Reason: Source constructors are not an ordered subset of target constructors"
            ':$$: 'Text "  Source constructors: " ':<>: 'ShowType sourceNames
            ':$$: 'Text "  Target constructors: " ':<>: 'ShowType targetNames
            ':$$: 'Text ""
            ':$$: 'Text "Solution: Keep the source constructors in order or write instance `ShapeCoercible "
                ':<>: 'ShowType x
                ':<>: 'Text " "
                ':<>: 'ShowType y
                ':<>: 'Text "`"
        )
    => GShapeWidening 'False sourceNames targetNames x y source target
    where
    gshapeWidening = error "unreachable"

class GWidenConstructors x y source target where
    gwidenConstructors :: source x -> target x

instance
    ( GWidenConstructors x y left target
    , GWidenConstructors x y right target
    )
    => GWidenConstructors x y (left :+: right) target
    where
    gwidenConstructors (L1 source) = gwidenConstructors @x @y source
    gwidenConstructors (R1 source) = gwidenConstructors @x @y source

instance
    GInjectConstructor
        x
        y
        name
        (M1 C ('MetaCons name fixity record) fields)
        target
    => GWidenConstructors
        x
        y
        (M1 C ('MetaCons name fixity record) fields)
        target
    where
    gwidenConstructors = ginjectConstructor @x @y @name

class GInjectConstructor x y (name :: Symbol) source target where
    ginjectConstructor :: source x -> target x

instance
    GInjectConstructorBranch
        (HasConstructor name left)
        x
        y
        name
        source
        left
        right
    => GInjectConstructor x y name source (left :+: right)
    where
    ginjectConstructor =
        gInjectConstructorBranch
            @(HasConstructor name left)
            @x
            @y
            @name

instance
    GShapeCoercible
        x
        y
        (M1 C ('MetaCons name sourceFixity sourceRecord) sourceFields)
        (M1 C ('MetaCons name targetFixity targetRecord) targetFields)
    => GInjectConstructor
        x
        y
        name
        (M1 C ('MetaCons name sourceFixity sourceRecord) sourceFields)
        (M1 C ('MetaCons name targetFixity targetRecord) targetFields)
    where
    ginjectConstructor = gscoerce @x @y

class
    GInjectConstructorBranch
        (inLeft :: Bool)
        x
        y
        (name :: Symbol)
        source
        left
        right
    where
    gInjectConstructorBranch :: source x -> (left :+: right) x

instance
    GInjectConstructor x y name source left
    => GInjectConstructorBranch 'True x y name source left right
    where
    gInjectConstructorBranch = L1 . ginjectConstructor @x @y @name

instance
    GInjectConstructor x y name source right
    => GInjectConstructorBranch 'False x y name source left right
    where
    gInjectConstructorBranch = R1 . ginjectConstructor @x @y @name

instance
    forall x y f1 f2 datatypeName a1 b1 c1 a2 b2 c2
     . GShapeDatatype (CmpNat (ConstructorCount f1) (ConstructorCount f2)) x y f1 f2
    => GShapeCoercible
        x
        y
        (M1 D ('MetaData datatypeName a1 b1 c1) f1)
        (M1 D ('MetaData datatypeName a2 b2 c2) f2)
    where
    gscoerce =
        M1
            . gshapeDatatype
                @(CmpNat (ConstructorCount f1) (ConstructorCount f2))
                @x
                @y
            . unM1

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
