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

data ServerSpec = ServerSpec
    { gadtName :: Name -- ^ Name of the GADT representing the command
    , apiName :: Name
    , serverName :: Name
    , endpoints :: [Endpoint] -- ^ Endpoints created from the constructors of the GADT
    } deriving Show

data Endpoint = Endpoint
    { fullConstructorName :: Name
    , shortConstructor :: String  -- ^ Name of the endpoint
    , handlerName :: Name
    , constructorArgs :: [Type]
    , constructorReturn :: Type
    , handlerReturn :: Type -- ^ Same as for constructor but with NoContent instead of ()
    } deriving Show

getCmdDec :: Name -> Q Dec
getCmdDec cmdName = do
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


getConstructors :: Dec -> Q [Con]
getConstructors = \case
    DataD _ _ [KindedTV _ StarT] _ constructors _ -> pure constructors
    DataD _ _ _ _ _ _ -> error "bad data type"
    _ -> error "Expected a GADT"

-- | Turn "ModuleA.ModuleB.Name" into "Name"
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
        let shortName = show $ unqualifiedName name
        hRetType <- unitToNoContent retType
        pure Endpoint { fullConstructorName = name
                      , shortConstructor    = shortName
                      , handlerName         = mkName $ lowerFirst shortName
                      , constructorArgs     = fmap snd bangArgs
                      , constructorReturn   = retType
                      , handlerReturn       = hRetType
                      }
    _ -> error "Expected a GATD constructor representing an endpoint"

-- | Turn "OhYEAH" into "ohYEAH"...
lowerFirst :: String -> String
lowerFirst = \case
    c : cs -> toLower c : cs
    []     -> []

unitToNoContent :: Type -> Q Type
unitToNoContent = \case
    TupleT 0 -> [t| NoContent |]
    t        -> pure t

-- | Create a ServerSpec from a GADT
-- The GADT must have one parameter representing the return type
mkServerSpec :: Name -> Q ServerSpec
mkServerSpec n = do
    eps <- traverse toEndpoint =<< getConstructors =<< getCmdDec n
    pure ServerSpec { gadtName   = n
                    , apiName    = mkName "Api"
                    , serverName = mkName "server"
                    , endpoints  = eps
                    }

-- | Create type aliases for each endpoint, using constructor name prefixed with "Ep",
-- and a type `Api` that represents the full API.
mkServer :: Name -> Q [Dec]
mkServer name = do
    serverSpec   <- mkServerSpec name
    endpointDecs <- traverse mkEndpointDec $ endpoints serverSpec
    handlers     <- mkHandlers serverSpec
    server       <- mkFullServer serverSpec
    pure $ endpointDecs <> handlers <> server

mkApiType :: [Endpoint] -> Q Type
mkApiType endpoints = case mkName . shortConstructor <$> endpoints of
    []     -> error "Server has no endpoints"
    x : xs -> do
        let f :: Type -> Name -> Q Type
            f b a = [t| $(pure b) :<|> $(pure $ ConT a) |]
        foldM f (ConT x) xs

-- | Create a typealias for the API
-- type Api = EpA :<|> EpB :<|> EpC ...
mkApiDec :: ServerSpec -> Q Dec
mkApiDec spec =
    TySynD (apiName spec) [] <$> case mkName . shortConstructor <$> endpoints spec of
        []     -> error "Server has no endpoints"
        x : xs -> do
            let f :: Type -> Name -> Q Type
                f b a = [t| $(pure b) :<|> $(pure $ ConT a) |]
            foldM f (ConT x) xs

-- | Create a request body by turning multiple arguments into a tuple
-- `BuyThing ItemKey Quantity` yields `ReqBody '[JSON] (ItemKey, Quantity)`
toReqBody :: [Type] -> Q Type
toReqBody args = do
    [t| ReqBody '[JSON] $(mkBody) |]
  where
    appAll :: Type -> [Type] -> Type
    appAll t = \case
        []     -> t
        x : xs -> appAll (AppT t x) xs

    mkBody :: Q Type
    mkBody = case args of
        []     -> error "Empty list cannot be turned into a tuple"
        a : [] -> pure a
        ts     -> pure $ appAll (TupleT $ length ts) ts


-- | Define the servant endpoint type. E.g.
-- "BuyBook" :> ReqBody '[JSON] (BookId, Integer) -> Post '[JSON] NoContent
epType :: Endpoint -> Q Type
epType e = [t| $(pure cmdName) :> $(reqBody) :> $(reqReturn) |]
  where
    cmdName :: Type
    cmdName = LitT . StrTyLit $ shortConstructor e

    reqBody :: Q Type
    reqBody = toReqBody $ constructorArgs e

    reqReturn :: Q Type
    reqReturn = [t| Post '[JSON] $(pure $ handlerReturn e) |]

-- | Define a type alias representing the type of the endpoint
mkEndpointDec :: Endpoint -> Q Dec
mkEndpointDec e = tySynD (mkName $ shortConstructor e) [] (epType e)



-- | Make command handlers for each endpoint
mkHandlers :: ServerSpec -> Q [Dec]
mkHandlers spec = fmap mconcat . traverse mkHandler $ endpoints spec
  where
    cmdRunner :: Name
    cmdRunner = mkName "cmdRunner"

    mkHandler :: Endpoint -> Q [Dec]
    mkHandler e = do
        cmdRunnerType  <- [t| CmdRunner $(pure . ConT $ gadtName spec) |]
        varNames       <- traverse (const $ newName "arg") $ constructorArgs e
        handlerRetType <- [t| Handler $(pure $ handlerReturn e) |]
        let varPat = TupP $ fmap VarP varNames
            nrArgs = length $ constructorArgs e
            funSig =
                SigD (handlerName e)
                    . AppT (AppT ArrowT cmdRunnerType)
                    $ case constructorArgs e of
                          []  -> handlerRetType
                          [a] -> AppT (AppT ArrowT a) handlerRetType
                          as  -> AppT
                              (AppT ArrowT (foldl AppT (TupleT (length as)) as))
                              handlerRetType

            funBodyBase = AppE (VarE cmdRunner)
                $ foldl AppE (ConE (fullConstructorName e)) (fmap VarE varNames)

            funBody = case constructorReturn e of
                TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
                _        -> pure funBodyBase
        funClause <- clause [pure (VarP cmdRunner), pure $ varPat]
                            (normalB [| liftIO $ $(funBody)  |])
                            []
        let funDef = FunD (handlerName e) [funClause]
        pure [funSig, funDef]

-- | The type of the CmdRunner used to execute commands
cmdRunnerType :: ServerSpec -> Q Type
cmdRunnerType spec = [t| CmdRunner $(pure . ConT $ gadtName spec) |]

-- | Create the full server and api dec
-- Assumes the handlers have already been created (using `mkHandlers`)
mkFullServer :: ServerSpec -> Q [Dec]
mkFullServer spec = do
    cmdRunnerT <- cmdRunnerType spec
    let cmdRunner = mkName "cmdRunner"

        handlerExp :: Endpoint -> Exp
        handlerExp e = VarE (handlerName e) `AppE` VarE cmdRunner

    body <- case handlerExp <$> endpoints spec of
        []     -> error "Server contains no endpoints"
        e : es -> foldM (\b a -> [| $(pure b) :<|> $(pure a) |]) e es
    apiDec        <- mkApiDec spec
    serverTypeDec <-
        SigD (serverName spec)
        .   AppT (AppT ArrowT cmdRunnerT)
        <$> [t| Server $(pure $ ConT $ apiName spec) |]
    let funDec = FunD (serverName spec) [Clause [VarP cmdRunner] (NormalB body) []]
    pure [apiDec, serverTypeDec, funDec]
