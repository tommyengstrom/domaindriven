{-# LANGUAGE TemplateHaskell #-}

module ServerSpec where

import           Language.Haskell.TH
import           DomainDriven.Server
import           DomainDriven
import           Prelude
import           GHC.Generics                   ( Generic )
import           Servant
import           Data.Aeson

newtype ItemKey = ItemKey String
    deriving newtype (Show, Eq, FromJSON, ToJSON)

newtype Price = Euros Int
    deriving newtype (Show, Eq, FromJSON, ToJSON)

data StoreCmd a where
   -- AddToCart    ::ItemKey -> StoreCmd ()
   -- CreateNewItem ::String -> Price -> StoreCmd ItemKey
   -- RemoveFromCart ::ItemKey -> StoreCmd ()
   -- Poke ::StoreCmd ()
    --ItemAction ::ItemCmd a -> StoreCmd a
    --ItemAction ::Int -> ItemCmd a -> StoreCmd a
    ItemAction ::Int -> String -> Bool -> ItemCmd a -> StoreCmd a

data ItemCmd a where
    SubOp1 ::ItemCmd ()
    SubOp2 ::Int -> String -> ItemCmd Int

data Item = Item
    { key :: ItemKey
    , description :: String
    , price :: Price
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data StoreLookup a where
    LookupAll ::StoreLookup [Item]
    LookupItem ::ItemKey -> StoreLookup Item

-- $(mkCmdServer ''StoreCmd)

$(mkQueryServer ''StoreLookup)
