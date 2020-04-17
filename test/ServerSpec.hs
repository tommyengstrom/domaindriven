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
    Poke ::StoreCmd ()
--    ItemSubCmd ::ItemKey -> SubCmd a -> StoreCmd a

data SubCmd a where
    SubOp1 ::SubCmd ()
    SubOp2 ::Int -> String -> SubCmd Int

data Item = Item
    { key :: ItemKey
    , description :: String
    , price :: Price
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data StoreLookup a where
    LookupAll ::StoreLookup [Item]
    Lookup ::ItemKey -> StoreLookup Item


$(mkServer ''StoreCmd)



-- type AddToCart
--     = (:>) "AddToCart" ((:>) (ReqBody '[JSON] ItemKey) (Post '[JSON] NoContent))
-- type CreateNewItem
--     = (:>) "CreateNewItem" ((:>) (ReqBody '[JSON] (String, Price)) (Post '[JSON] ItemKey))
-- type RemoveFromCart
--     = (:>) "RemoveFromCart" ((:>) (ReqBody '[JSON] ItemKey) (Post '[JSON] NoContent))
-- type Poke = (:>) "Poke" (Post '[JSON] NoContent)
-- type ItemSubCmd
--     = (:>)
--           "ItemSubCmd"
--           ((:>) (ReqBody '[JSON] (ItemKey, SubCmd a_a7ME)) (Post '[JSON] a_a7ME))
-- addToCart :: CmdRunner StoreCmd -> ItemKey -> Handler NoContent
-- addToCart cmdRunner (arg_amOh) =
--     (liftIO $ (fmap (const NoContent)) (cmdRunner (AddToCart arg_amOh)))
-- createNewItem :: CmdRunner StoreCmd -> (String, Price) -> Handler ItemKey
-- createNewItem cmdRunner (arg_amOi, arg_amOj) =
--     (liftIO $ cmdRunner ((CreateNewItem arg_amOi) arg_amOj))
-- removeFromCart :: CmdRunner StoreCmd -> ItemKey -> Handler NoContent
-- removeFromCart cmdRunner (arg_amOk) =
--     (liftIO $ (fmap (const NoContent)) (cmdRunner (RemoveFromCart arg_amOk)))
-- poke :: CmdRunner StoreCmd -> Handler NoContent
-- poke cmdRunner = (liftIO $ (fmap (const NoContent)) (cmdRunner Poke))
-- itemSubCmd :: CmdRunner StoreCmd -> ItemKey -> SubCmd a_a7ME) -> Handler a_a7ME
-- itemSubCmd cmdRunner (arg_amOl, arg_amOm) =
--     (liftIO $ cmdRunner ((ItemSubCmd arg_amOl) arg_amOm))
-- type Api
--     = (:<|>)
--           ((:<|>) ((:<|>) ((:<|>) AddToCart CreateNewItem) RemoveFromCart) Poke)
--           ItemSubCmd
--

-- ForallC
--   [KindedTV a StarT]
--   []
--   (GadtC
--      [ServerSpec.ItemSubCmd]
--      [ (Bang NoSourceUnpackedness NoSourceStrictness, ConT ServerSpec.ItemKey)
--      , ( Bang NoSourceUnpackedness NoSourceStrictness
--        , AppT (ConT ServerSpec.SubCmd) (VarT a))
--      ]
--      (AppT (ConT ServerSpec.StoreCmd) (VarT a)))
