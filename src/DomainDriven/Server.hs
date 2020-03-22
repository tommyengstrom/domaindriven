{-# LANGUAGE TemplateHaskell #-}
module DomainDriven.Server where

import           DomainDriven.Internal.Class
import           Prelude
import           Language.Haskell.TH
import           Control.Monad
import           Data.List                      ( unfoldr )
import           Servant

-- The first goal is to generate a server from a `CmdHandler model event cmd err`. Later
-- on I will refactor queris to alsu use a GADT and follow the same pattern.
--
-- In order to achieve this I will
-- * Start by writing the template haskell to generate endpoint types from a `Cmd a`
-- * All arguments will be encoded in the request body
-- * All commands use POST or PUT (does one make more sense?)
-- * Ensure that error messages are easy to understand!
--
--
data StoreCmd a where
    BuyItem    ::String -> Int -> StoreCmd ()
-------------------------------------------------
-------------  The expected info
-------------------------------------------------
--TyConI
--  (DataD
--     []
--     DomainDriven.Server.StoreCmd
--     [KindedTV a_6989586621679127356 StarT]
--     Nothing
--     [ GadtC
--         [DomainDriven.Server.BuyItem]
--         [ (Bang NoSourceUnpackedness NoSourceStrictness, ConT GHC.Base.String)
--         , (Bang NoSourceUnpackedness NoSourceStrictness, ConT GHC.Types.Int)
--         ]
--         (AppT (ConT DomainDriven.Server.StoreCmd) (TupleT 0))
--     ]
--     [])
-------------------------------------------------
-------------  The Dec it contains
-------------------------------------------------
-- DataD
--   []
--   DomainDriven.Server.StoreCmd
--   [KindedTV a_6989586621679127607 StarT]
--   Nothing
--   [ GadtC
--       [DomainDriven.Server.BuyItem]
--       [ (Bang NoSourceUnpackedness NoSourceStrictness, ConT GHC.Base.String)
--       , (Bang NoSourceUnpackedness NoSourceStrictness, ConT GHC.Types.Int)
--       ]
--       (AppT (ConT DomainDriven.Server.StoreCmd) (TupleT 0))
--   ]
--   []

getDec :: Name -> Q Dec
getDec cmdName = do
    cmdType <- reify cmdName
    let errMsg = error "Type must be GADT with one parameter (return type)"
    case cmdType of
        TyConI dec       -> pure dec
        ClassI _ _       -> errMsg
        ClassOpI _ _ _   -> errMsg
        FamilyI _ _      -> errMsg
        PrimTyConI _ _ _ -> errMsg
        DataConI   _ _ _ -> errMsg
        PatSynI _ _      -> errMsg
        VarI _ _ _       -> errMsg
        TyVarI _ _       -> errMsg


data Endpoint = Endpoint
    { epName :: String
    , epArgs :: [Type]
    , epReturn :: Type
    } deriving Show

getConstructors :: Dec -> Q [Con]
getConstructors = \case
    DataD _ _ [KindedTV _ StarT] _ constructors _ -> pure constructors
    DataD _ _ _ _ _ _ -> error "bad data type"
    _ -> error "Expected a GADT"

unqualifiedName :: Name -> Name
unqualifiedName name = case reverse $ unfoldr f (show name) of
    a : _ -> mkName a
    _     -> name
  where
    f :: String -> Maybe (String, String)
    f s = case span (/= '.') s of
        ("", _         ) -> Nothing
        (x , '.' : rest) -> Just (x, rest)
        (x , rest      ) -> Just (x, rest)

getParameters :: Con -> Q Endpoint
getParameters = \case
    GadtC [name] bangArgs retType -> pure Endpoint { epName = show $ unqualifiedName name
                                                   , epArgs = fmap snd bangArgs
                                                   , epReturn = retType
                                                   }
    _ -> error "That's dog shit" -- FIXME: Write nice error messages!


mkEndpoints :: Name -> Q [Dec]
mkEndpoints =
    traverse mkEndpoint <=< traverse getParameters <=< getConstructors <=< getDec
-- runQ $ [d| type EP = "endpoint" :> Get '[JSON] Int |]
--
-- [ TySynD
--     EP_0
--     []
--     (AppT
--        (AppT (ConT Servant.API.Sub.:>) (LitT (StrTyLit "endpoint")))
--        (AppT
--           (AppT
--              (ConT Servant.API.Verbs.Get)
--              (AppT
--                 (AppT PromotedConsT (ConT Servant.API.ContentTypes.JSON))
--                 PromotedNilT))
--           (ConT GHC.Types.Int)))
-- ]

-- | The name of the type alias representing the endpoint
epTypeName :: Endpoint -> Name
epTypeName = mkName . mappend "Ep" . epName

--runQ [t| "BuyBook" :> Post '[JSON] NoContent |]
--AppT
--  (AppT (ConT Servant.API.Sub.:>) (LitT (StrTyLit "BuyBook")))
--  (AppT
--     (AppT
--        (ConT Servant.API.Verbs.Post)
--        (AppT
--           (AppT PromotedConsT (ConT Servant.API.ContentTypes.JSON))
--           PromotedNilT))
--     (ConT Servant.API.ContentTypes.NoContent))

mkReqBody :: [Type] -> Q Type
mkReqBody = \case
    []     -> error "Empty list cannot be turned into a tuple"
    a : [] -> pure a
    ts     -> pure $ appAll (TupleT $ length ts) ts
  where
    appAll :: Type -> [Type] -> Type
    appAll t = \case
        []     -> t
        x : xs -> appAll (AppT t x) xs


-- | Define the servant endpoint type. E.g.
-- "BuyBook" :> ReqBody '[JSON] (BookId, Integer) -> Post '[JSON] NoContent
epType :: Endpoint -> Q Type
epType e = appT (appT bird nameAndBody) reqReturn
  where
    -- "Something" :> ReqBody '[JSON] Something
    nameAndBody :: Q Type
    nameAndBody = appT (appT bird (pure cmdName)) reqBody


    cmdName :: Type
    cmdName = LitT . StrTyLit $ epName e

    reqBody :: Q Type
    reqBody = appT [t| ReqBody '[JSON] |] (mkReqBody $ epArgs e)

    reqReturn :: Q Type
    reqReturn = appT [t| Post '[JSON] |] (pure $ epReturn e)


    -- The bird operator, aka :>
    bird :: Q Type
    bird = [t| (:>) |]

-- | Define a type alias representing the type of the endpoint
mkEndpoint :: Endpoint -> Q Dec
mkEndpoint e = tySynD (epTypeName e) [] (epType e)
