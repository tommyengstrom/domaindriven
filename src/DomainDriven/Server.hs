{-# LANGUAGE TemplateHaskell #-}
module DomainDriven.Server
    ( module DomainDriven.Server
    , liftIO
    )
where

import           DomainDriven.Internal.Class
import           Prelude
import           Language.Haskell.TH
import           Control.Monad
import           Data.List                      ( unfoldr )
import           Servant
import           Debug.Trace
import           Data.Char
import           Control.Monad.Trans
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
    AddToCart    ::String -> Int -> StoreCmd [Int]
    RemoveFromCart ::String -> StoreCmd ()

getDec :: Name -> Q Dec
getDec cmdName = do
    cmdType <- reify cmdName
    let errMsg = error "Must be GADT with one parameter, representing return type."
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
    , epTypeAliasName :: String
    , constructorName :: Name
    , constructorArgs :: [Type]
    , constructorReturn :: Type
    } deriving Show

-- | Turn "ModuleA.ModuleB.Name" into "Name"
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

toEndpoint :: Con -> Q Endpoint
toEndpoint = \case
    GadtC [name] bangArgs (AppT _ retType) -> do
        let baseName = show $ unqualifiedName name
        pure Endpoint { epName            = baseName
                      , constructorName   = name
                      , epTypeAliasName   = "Ep" <> baseName
                      , constructorArgs   = fmap snd bangArgs
                      , constructorReturn = retType
                      }
    _ -> error "Expected a GATD constructor representing an endpoint"

epReturnType :: Type -> Q Type
epReturnType = \case
    TupleT 0 -> [t| NoContent |]
    t        -> pure t

-- | Create the type aliases representing the endpoints
mkEndpointDecs :: Name -> Q [Dec]
mkEndpointDecs =
    traverse mkEndpointDec <=< traverse toEndpoint <=< getConstructors <=< getDec

getEndpoints :: Name -> Q [Endpoint]
getEndpoints = traverse toEndpoint <=< getConstructors <=< getDec

-- | Create type aliases for each endpoint, using constructor name prefixed with "Ep",
-- and a type `Api` that represents the full API.
mkApiDec :: Name -> Q [Dec]
mkApiDec name = do
    endpoints     <- getEndpoints name
    endpointDecs  <- traverse mkEndpointDec endpoints
    serverTypeDec <- TySynD (mkName "Api") [] <$> mkApiType endpoints
    handlers      <- mconcat <$> traverse (mkApiHandlerDec name) endpoints
    server        <- mkFullServer endpoints
    pure $ serverTypeDec : server : endpointDecs <> handlers

mkApiType :: [Endpoint] -> Q Type
mkApiType endpoints = case mkName . epTypeAliasName <$> reverse endpoints of
    []     -> error "Server has no endpoints"
    x : xs -> do
        let f :: Type -> Name -> Q Type
            f b a = appT (appT [t| (:<|>) |] (pure $ ConT a)) (pure b)
        foldM f (ConT x) xs

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
    reqBody = appT [t| ReqBody '[JSON] |] (mkReqBody $ constructorArgs e)

    reqReturn :: Q Type
    reqReturn = appT [t| Post '[JSON] |] (pure $ constructorReturn e)


    -- The bird operator, aka :>
    bird :: Q Type
    bird = [t| (:>) |]

-- | Define a type alias representing the type of the endpoint
mkEndpointDec :: Endpoint -> Q Dec
mkEndpointDec e = tySynD (mkName $ epTypeAliasName e) [] (epType e)


lowerFirst :: String -> String
lowerFirst = \case
    c : cs -> toLower c : cs
    []     -> []

-- | Generate a handler for the endpoint
-- Constructor `AddToCart :: String -> Int -> StoreCmd ()` will result in:

appMany :: Type -> [Type] -> Type
appMany t args = foldl AppT t args

--a `addToCart :: (String, Int) -> Handler ()`

type CmdGADT = Name

mkApiHandlerDec :: CmdGADT -> Endpoint -> Q [Dec]
mkApiHandlerDec cmdType e = do
    traceShowM e
    let handlerName = mkName . lowerFirst $ epName e :: Name
        cmdRunner   = mkName "cmdRunner"
    cmdRunnerType  <- [t| CmdRunner $(pure $ ConT cmdType) |]
    varNames       <- traverse (\_ -> newName "arg") $ constructorArgs e
    handlerRetType <- appT [t| Handler |] (epReturnType $ constructorReturn e)
    let varPat = TupP $ fmap VarP varNames
        nrArgs = length $ constructorArgs e
        funSig =
            SigD (mkName $ lowerFirst $ epName e)
                . AppT (AppT ArrowT cmdRunnerType)
                $ case constructorArgs e of
                      []  -> handlerRetType
                      [a] -> AppT (AppT ArrowT a) handlerRetType
                      as  -> AppT (AppT ArrowT (foldl AppT (TupleT (length as)) as))
                                  handlerRetType

        funBodyBase = AppE (VarE cmdRunner)
            $ foldl AppE (ConE (constructorName e)) (fmap VarE varNames)

        funBody = case constructorReturn e of
            TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
            _        -> pure funBodyBase
    funClause <- clause [pure (VarP cmdRunner), pure $ varPat]
                        (normalB [| liftIO $ $(funBody)  |])
                        []
    let funDef = FunD handlerName [funClause]
    pure [funSig, funDef]


-- This must pass the first argument (CmdRunner a) to each handler!
mkFullServer :: [Endpoint] -> Q Dec
mkFullServer endpoints = do
    b <- case VarE . mkName . lowerFirst . epName <$> endpoints of
        []     -> error "Server contains no endpoints"
        e : es -> foldM (\b a -> [| $(pure b) :<|> $(pure a) |]) e es
    pure $ FunD (mkName "server") [Clause [] (NormalB b) []]
