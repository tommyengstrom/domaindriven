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
    Lookup ::ItemKey -> StoreLookup Item


-- $(mkServer ''StoreCmd)
type SubOp1 = (:>) "SubOp1" (Post '[JSON] NoContent)

type SubOp2 = (:>) "SubOp2" ((:>) (ReqBody '[JSON] (Int, String)) (Post '[JSON] Int))

subOp1 :: CmdRunner ItemCmd -> Handler NoContent
subOp1 cmdRunner = (liftIO $ (fmap (const NoContent)) (cmdRunner SubOp1))

subOp2 :: CmdRunner ItemCmd -> (Int, String) -> Handler Int
subOp2 cmdRunner (arg_asfl, arg_asfm) = (liftIO $ cmdRunner ((SubOp2 arg_asfl) arg_asfm))

type ItemActionApi = (:<|>) SubOp1 SubOp2

itemActionServer :: CmdRunner ItemCmd -> Server ItemActionApi
itemActionServer cmdRunner = (subOp1 cmdRunner :<|> subOp2 cmdRunner)

type ItemAction
    = (:>)
          "ItemAction"
          ( (:>)
                ( (:>)
                      ((:>) (Capture "typename" Int) (Capture "typename" String))
                      (Capture "typename" Bool)
                )
                ItemActionApi
          )

--- This is not the form we get them in! We get a GADT constructor. Fix it!
itemAction :: CmdRunner StoreCmd -> Int -> String -> Bool -> Server ItemActionApi
itemAction cmdRunner_asfn arg_asfo arg_asfp arg_asfq =
    itemActionServer (cmdRunner_asfn . ItemAction arg_asfo arg_asfp arg_asfq)

type Api = ItemAction

server :: CmdRunner StoreCmd -> Server Api
server cmdRunner = itemAction cmdRunner
