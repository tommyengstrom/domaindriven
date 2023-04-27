{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Server.Servant.Helper.GenericRecord where

import Data.Kind
import GHC.TypeLits
import Generics.SOP.GGP
import Generics.SOP.Type.Metadata

type family GenericRecordFields (record :: Type) :: [Type] where
    GenericRecordFields record = GenericRecordFields' (GCode record)

type family GenericRecordFields' (code :: [[Type]]) :: [Type] where
    GenericRecordFields' '[fields] = fields
    GenericRecordFields' _ = TypeError ('Text "not a record!")

type family GenericRecordFieldInfos (record :: Type) :: [FieldInfo] where
    GenericRecordFieldInfos record = GenericRecordFieldInfos' (GDatatypeInfoOf record)

type family GenericRecordFieldInfos' (info :: DatatypeInfo) :: [FieldInfo] where
    GenericRecordFieldInfos' ('ADT _ _ '[ 'Record _ infos] _) = infos
    GenericRecordFieldInfos' _ = TypeError ('Text "not a record!")
