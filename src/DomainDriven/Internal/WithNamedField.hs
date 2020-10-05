{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}

module DomainDriven.Internal.WithNamedField where

import           Prelude
import           DomainDriven.Internal.JsonFieldName
import           GHC.Generics
import           Data.Aeson
import           Data.Text                      ( Text )
import qualified Data.Text                                    as T
import qualified Data.HashMap.Strict                          as HM


class GNamedToJSON a where
    gNamedToJSON :: a x -> [(Text, Value)]

instance (GNamedToJSON f, Datatype d) => GNamedToJSON (M1 D d f) where
    gNamedToJSON = gNamedToJSON . unM1

instance (GNamedToJSON f, Constructor c) => GNamedToJSON (M1 C c f) where
    gNamedToJSON a = [("tag", String . T.pack $ conName a)] <> gNamedToJSON (unM1 a)
    -- FIXME: check conIsRecord

instance (JsonFieldName t) => GNamedToJSON (M1 S c (Rec0 t)) where
    gNamedToJSON a = [(fieldName @t, toJSON . unK1 $ unM1 a)]

instance GNamedToJSON U1 where
    gNamedToJSON U1 = []

instance (GNamedToJSON a, GNamedToJSON b) => GNamedToJSON (a :*: b) where
    gNamedToJSON (a :*: b) = gNamedToJSON a <> gNamedToJSON b

instance (GNamedToJSON a, GNamedToJSON b) => GNamedToJSON (a :+: b) where
    gNamedToJSON = \case
        L1 a -> gNamedToJSON a
        R1 a -> gNamedToJSON a


--------- Testing it ----------------
data Apa = Apan
    deriving (Show, Eq, Ord, Generic)

data Bulle = Middag
    | Beer Int String
    | Kaffe Text
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Apa where
    toJSON = Object . HM.fromList . gNamedToJSON . from

instance ToJSON Bulle where
    toJSON = Object . HM.fromList . gNamedToJSON . from


test :: IO ()
test = do
    print $ encode $ Apan
    print $ encode Middag
    print $ encode $ Beer 2 "bulle"
    print $ encode $ Kaffe "kaffe!"

bulleRep :: Rep Bulle x
bulleRep = from $ Kaffe "hejsan"
