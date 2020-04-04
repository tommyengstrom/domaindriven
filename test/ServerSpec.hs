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
    AddToCart    ::ItemKey -> StoreCmd ()
    CreateNewItem ::String -> Price -> StoreCmd ItemKey
    RemoveFromCart ::ItemKey -> StoreCmd ()

data Item = Item
    { key :: ItemKey
    , description :: String
    , price :: Price
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data StoreLookup a where
    LookupAll ::StoreLookup [Item]
    Lookup ::ItemKey -> StoreLookup Item


$(mkServer ''StoreCmd)
