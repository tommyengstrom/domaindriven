{-# LANGUAGE AllowAmbiguousTypes #-}

module DomainDriven.Internal.NamedFields
    ( NamedFields1 (..)
    , NamedFields2 (..)
    , NamedFields3 (..)
    , NamedFields4 (..)
    , NamedFields5 (..)
    , NamedFields6 (..)
    , NamedFields7 (..)
    , NamedFields8 (..)
    , NamedFields9 (..)
    , NamedFields10 (..)
    ) where

import           Data.Aeson
import           Data.OpenApi
import           DomainDriven.Internal.HasFieldName
import           Prelude
import           GHC.Generics                   ( Generic )
import           Data.Proxy
import           DomainDriven.Internal.NamedJsonFields
import           GHC.TypeLits

opts :: forall name . KnownSymbol name => NamedJsonOptions
opts = defaultNamedJsonOptions { skipTagField         = True
                               , datatypeNameModifier = const (symbolVal $ Proxy @name)
                               }

---------------------------------------------------------------
data NamedFields1 (name :: Symbol) a  = NamedFields1 a
    deriving (Show, Eq, Ord, Generic)

instance (KnownSymbol name
        , ToJSON a, HasFieldName a)
        =>  ToJSON (NamedFields1 name a ) where
    toJSON = gNamedToJson (opts @name)

instance (KnownSymbol name
        , ToSchema a, HasFieldName a)
    =>  ToSchema (NamedFields1 name a ) where
    declareNamedSchema = gNamedDeclareNamedSchema (opts @name)

instance (KnownSymbol name
        , FromJSON a, HasFieldName a)
    =>  FromJSON (NamedFields1 name a ) where
    parseJSON = gNamedParseJson (opts @name)
---------------------------------------------------------------
data NamedFields2 (name :: Symbol) a b = NamedFields2 a b
    deriving (Show, Eq, Ord, Generic)

instance (KnownSymbol name
        , ToJSON a, HasFieldName a
        , ToJSON b, HasFieldName b)
        =>  ToJSON (NamedFields2 name a b) where
    toJSON = gNamedToJson (opts @name)

instance (KnownSymbol name
        , ToSchema a, HasFieldName a
        , ToSchema b, HasFieldName b)
    =>  ToSchema (NamedFields2 name a b) where
    declareNamedSchema = gNamedDeclareNamedSchema (opts @name)

instance (KnownSymbol name
        , FromJSON a, HasFieldName a
        , FromJSON b, HasFieldName b)
    =>  FromJSON (NamedFields2 name a b) where
    parseJSON = gNamedParseJson (opts @name)

---------------------------------------------------------------
data NamedFields3 (name :: Symbol) a b c = NamedFields3 a b c
    deriving (Show, Eq, Ord, Generic)

instance (KnownSymbol name
        , ToJSON a, HasFieldName a
        , ToJSON b, HasFieldName b
        , ToJSON c, HasFieldName c)
        =>  ToJSON (NamedFields3 name a b c) where
    toJSON = gNamedToJson (opts @name)

instance (KnownSymbol name
        , ToSchema a, HasFieldName a
        , ToSchema b, HasFieldName b
        , ToSchema c, HasFieldName c)
    =>  ToSchema (NamedFields3 name a b c) where
    declareNamedSchema = gNamedDeclareNamedSchema (opts @name)

instance (KnownSymbol name
        , FromJSON a, HasFieldName a
        , FromJSON b, HasFieldName b
        , FromJSON c, HasFieldName c)
    =>  FromJSON (NamedFields3 name a b c) where
    parseJSON = gNamedParseJson (opts @name)

---------------------------------------------------------------
data NamedFields4 (name :: Symbol) a b c d = NamedFields4 a b c d
    deriving (Show, Eq, Ord, Generic)

instance (KnownSymbol name
        , ToJSON a, HasFieldName a
        , ToJSON b, HasFieldName b
        , ToJSON c, HasFieldName c
        , ToJSON d, HasFieldName d)
        =>  ToJSON (NamedFields4 name a b c d) where
    toJSON = gNamedToJson (opts @name)

instance (KnownSymbol name
        , ToSchema a, HasFieldName a
        , ToSchema b, HasFieldName b
        , ToSchema c, HasFieldName c
        , ToSchema d, HasFieldName d)
    =>  ToSchema (NamedFields4 name a b c d) where
    declareNamedSchema = gNamedDeclareNamedSchema (opts @name)

instance (KnownSymbol name
        , FromJSON a, HasFieldName a
        , FromJSON b, HasFieldName b
        , FromJSON c, HasFieldName c
        , FromJSON d, HasFieldName d)
    =>  FromJSON (NamedFields4 name a b c d) where
    parseJSON = gNamedParseJson (opts @name)


---------------------------------------------------------------
data NamedFields5 (name :: Symbol) a b c d e = NamedFields5 a b c d e
    deriving (Show, Eq, Ord, Generic)

instance (KnownSymbol name
        , ToJSON a, HasFieldName a
        , ToJSON b, HasFieldName b
        , ToJSON c, HasFieldName c
        , ToJSON d, HasFieldName d
        , ToJSON e, HasFieldName e)
        =>  ToJSON (NamedFields5 name a b c d e) where
    toJSON = gNamedToJson (opts @name)

instance (KnownSymbol name
        , ToSchema a, HasFieldName a
        , ToSchema b, HasFieldName b
        , ToSchema c, HasFieldName c
        , ToSchema d, HasFieldName d
        , ToSchema e, HasFieldName e)
    =>  ToSchema (NamedFields5 name a b c d e) where
    declareNamedSchema = gNamedDeclareNamedSchema (opts @name)

instance (KnownSymbol name
        , FromJSON a, HasFieldName a
        , FromJSON b, HasFieldName b
        , FromJSON c, HasFieldName c
        , FromJSON d, HasFieldName d
        , FromJSON e, HasFieldName e)
    =>  FromJSON (NamedFields5 name a b c d e) where
    parseJSON = gNamedParseJson (opts @name)


---------------------------------------------------------------
data NamedFields6 (name :: Symbol) a b c d e f = NamedFields6 a b c d e f
    deriving (Show, Eq, Ord, Generic)

instance (KnownSymbol name
        , ToJSON a, HasFieldName a
        , ToJSON b, HasFieldName b
        , ToJSON c, HasFieldName c
        , ToJSON d, HasFieldName d
        , ToJSON e, HasFieldName e
        , ToJSON f, HasFieldName f)
        =>  ToJSON (NamedFields6 name a b c d e f) where
    toJSON = gNamedToJson (opts @name)

instance (KnownSymbol name
        , ToSchema a, HasFieldName a
        , ToSchema b, HasFieldName b
        , ToSchema c, HasFieldName c
        , ToSchema d, HasFieldName d
        , ToSchema e, HasFieldName e
        , ToSchema f, HasFieldName f)
    =>  ToSchema (NamedFields6 name a b c d e f) where
    declareNamedSchema = gNamedDeclareNamedSchema (opts @name)

instance (KnownSymbol name
        , FromJSON a, HasFieldName a
        , FromJSON b, HasFieldName b
        , FromJSON c, HasFieldName c
        , FromJSON d, HasFieldName d
        , FromJSON e, HasFieldName e
        , FromJSON f, HasFieldName f)
    =>  FromJSON (NamedFields6 name a b c d e f) where
    parseJSON = gNamedParseJson (opts @name)

---------------------------------------------------------------
data NamedFields7 (name :: Symbol) a b c d e f g = NamedFields7 a b c d e f g
    deriving (Show, Eq, Ord, Generic)

instance (KnownSymbol name
        , ToJSON a, HasFieldName a
        , ToJSON b, HasFieldName b
        , ToJSON c, HasFieldName c
        , ToJSON d, HasFieldName d
        , ToJSON e, HasFieldName e
        , ToJSON f, HasFieldName f
        , ToJSON g, HasFieldName g)
        =>  ToJSON (NamedFields7 name a b c d e f g) where
    toJSON = gNamedToJson (opts @name)

instance (KnownSymbol name
        , ToSchema a, HasFieldName a
        , ToSchema b, HasFieldName b
        , ToSchema c, HasFieldName c
        , ToSchema d, HasFieldName d
        , ToSchema e, HasFieldName e
        , ToSchema f, HasFieldName f
        , ToSchema g, HasFieldName g)
    =>  ToSchema (NamedFields7 name a b c d e f g) where
    declareNamedSchema = gNamedDeclareNamedSchema (opts @name)

instance (KnownSymbol name
        , FromJSON a, HasFieldName a
        , FromJSON b, HasFieldName b
        , FromJSON c, HasFieldName c
        , FromJSON d, HasFieldName d
        , FromJSON e, HasFieldName e
        , FromJSON f, HasFieldName f
        , FromJSON g, HasFieldName g)
    =>  FromJSON (NamedFields7 name a b c d e f g) where
    parseJSON = gNamedParseJson (opts @name)
---------------------------------------------------------------
data NamedFields8 (name :: Symbol) a b c d e f g h = NamedFields8 a b c d e f g h
    deriving (Show, Eq, Ord, Generic)

instance (KnownSymbol name
        , ToJSON a, HasFieldName a
        , ToJSON b, HasFieldName b
        , ToJSON c, HasFieldName c
        , ToJSON d, HasFieldName d
        , ToJSON e, HasFieldName e
        , ToJSON f, HasFieldName f
        , ToJSON g, HasFieldName g
        , ToJSON h, HasFieldName h)
        =>  ToJSON (NamedFields8 name a b c d e f g h) where
    toJSON = gNamedToJson (opts @name)

instance (KnownSymbol name
        , ToSchema a, HasFieldName a
        , ToSchema b, HasFieldName b
        , ToSchema c, HasFieldName c
        , ToSchema d, HasFieldName d
        , ToSchema e, HasFieldName e
        , ToSchema f, HasFieldName f
        , ToSchema g, HasFieldName g
        , ToSchema h, HasFieldName h)
    =>  ToSchema (NamedFields8 name a b c d e f g h) where
    declareNamedSchema = gNamedDeclareNamedSchema (opts @name)

instance (KnownSymbol name
        , FromJSON a, HasFieldName a
        , FromJSON b, HasFieldName b
        , FromJSON c, HasFieldName c
        , FromJSON d, HasFieldName d
        , FromJSON e, HasFieldName e
        , FromJSON f, HasFieldName f
        , FromJSON g, HasFieldName g
        , FromJSON h, HasFieldName h)
    =>  FromJSON (NamedFields8 name a b c d e f g h) where
    parseJSON = gNamedParseJson (opts @name)
---------------------------------------------------------------
data NamedFields9 (name :: Symbol) a b c d e f g h i = NamedFields9 a b c d e f g h i
    deriving (Show, Eq, Ord, Generic)

instance (KnownSymbol name
        , ToJSON a, HasFieldName a
        , ToJSON b, HasFieldName b
        , ToJSON c, HasFieldName c
        , ToJSON d, HasFieldName d
        , ToJSON e, HasFieldName e
        , ToJSON f, HasFieldName f
        , ToJSON g, HasFieldName g
        , ToJSON h, HasFieldName h
        , ToJSON i, HasFieldName i)
        =>  ToJSON (NamedFields9 name a b c d e f g h i) where
    toJSON = gNamedToJson (opts @name)

instance (KnownSymbol name
        , ToSchema a, HasFieldName a
        , ToSchema b, HasFieldName b
        , ToSchema c, HasFieldName c
        , ToSchema d, HasFieldName d
        , ToSchema e, HasFieldName e
        , ToSchema f, HasFieldName f
        , ToSchema g, HasFieldName g
        , ToSchema h, HasFieldName h
        , ToSchema i, HasFieldName i)
    =>  ToSchema (NamedFields9 name a b c d e f g h i) where
    declareNamedSchema = gNamedDeclareNamedSchema (opts @name)

instance (KnownSymbol name
        , FromJSON a, HasFieldName a
        , FromJSON b, HasFieldName b
        , FromJSON c, HasFieldName c
        , FromJSON d, HasFieldName d
        , FromJSON e, HasFieldName e
        , FromJSON f, HasFieldName f
        , FromJSON g, HasFieldName g
        , FromJSON h, HasFieldName h
        , FromJSON i, HasFieldName i)
    =>  FromJSON (NamedFields9 name a b c d e f g h i) where
    parseJSON = gNamedParseJson (opts @name)
---------------------------------------------------------------
data NamedFields10 (name :: Symbol) a b c d e f g h i j = NamedFields10 a b c d e f g h i j
    deriving (Show, Eq, Ord, Generic)

instance (KnownSymbol name
        , ToJSON a, HasFieldName a
        , ToJSON b, HasFieldName b
        , ToJSON c, HasFieldName c
        , ToJSON d, HasFieldName d
        , ToJSON e, HasFieldName e
        , ToJSON f, HasFieldName f
        , ToJSON g, HasFieldName g
        , ToJSON h, HasFieldName h
        , ToJSON i, HasFieldName i
        , ToJSON j, HasFieldName j)
        =>  ToJSON (NamedFields10 name a b c d e f g h i j) where
    toJSON = gNamedToJson (opts @name)

instance (KnownSymbol name
        , ToSchema a, HasFieldName a
        , ToSchema b, HasFieldName b
        , ToSchema c, HasFieldName c
        , ToSchema d, HasFieldName d
        , ToSchema e, HasFieldName e
        , ToSchema f, HasFieldName f
        , ToSchema g, HasFieldName g
        , ToSchema h, HasFieldName h
        , ToSchema i, HasFieldName i
        , ToSchema j, HasFieldName j)
    =>  ToSchema (NamedFields10 name a b c d e f g h i j) where
    declareNamedSchema = gNamedDeclareNamedSchema (opts @name)

instance (KnownSymbol name
        , FromJSON a, HasFieldName a
        , FromJSON b, HasFieldName b
        , FromJSON c, HasFieldName c
        , FromJSON d, HasFieldName d
        , FromJSON e, HasFieldName e
        , FromJSON f, HasFieldName f
        , FromJSON g, HasFieldName g
        , FromJSON h, HasFieldName h
        , FromJSON i, HasFieldName i
        , FromJSON j, HasFieldName j)
    =>  FromJSON (NamedFields10 name a b c d e f g h i j) where
    parseJSON = gNamedParseJson (opts @name)












