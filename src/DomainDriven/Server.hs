{-# LANGUAGE TemplateHaskell #-}
module DomainDriven.Server
    ( module DomainDriven.Server
    , liftIO
    , module DomainDriven.Internal.NamedFields
    , JsonFieldName(..)
    )
where

import           DomainDriven.Internal.Class
import           DomainDriven.Internal.JsonFieldName (JsonFieldName(..))
import           Prelude
import           Language.Haskell.TH
import           Control.Monad
import           Data.List                      ( unfoldr )
import           Servant
import           Data.Char
import           Control.Monad.Trans
import           Control.Lens
import           Data.Generics.Product
import           GHC.Generics                   ( Generic )
import           DomainDriven.Internal.NamedFields

data ServerSpec = ServerSpec
    { gadtName :: Name -- ^ Name of the GADT representing the command
    , apiName :: Name
    , serverName :: Name
    , endpoints :: [Endpoint] -- ^ Endpoints created from the constructors of the GADT
    , serverType :: ServerType
    } deriving (Show, Generic)

data ServerType
    = CmdServer -- ^ A server that runs commands, i.e. requires a command runner to run
    | QueryServer -- ^ A server that runs queries.
    deriving (Show, Generic)

data Endpoint
    = Endpoint EndpointData
    | SubApi SubApiData
    deriving (Show, Generic)


data EndpointData = EndpointData
    { eFullConstructorName :: Name
    , eShortConstructor :: String  -- ^ Name of the endpoint
    , eHandlerName :: Name
    , eConstructorArgs :: [Type]
    , eConstructorReturns :: Type
    , eHandlerReturn :: Type -- ^ Same as for constructor but with NoContent instead of ()
    } deriving (Show, Generic)

data SubApiData = SubApiData
    { feFullConstructorName :: Name
    , feShortConstructor :: String  -- ^ Name of the endpoint
    , feHandlerName :: Name
    , feConstructorArgs :: [Type]
    , feSubCmd :: Name
    , feSubApiName :: Name
    , feSubServerName :: Name
    } deriving (Show, Generic)

data ServerOptions = ServerOptions
    { renameConstructor :: String -> String
    } deriving (Generic)

defaultServerOptions :: ServerOptions
defaultServerOptions = ServerOptions
    { renameConstructor = id
    }

fullConstructorName :: Lens' Endpoint Name
fullConstructorName = lens getter setter
  where
    getter = \case
        Endpoint a -> eFullConstructorName a
        SubApi   a -> feFullConstructorName a

    setter s b = case s of
        Endpoint a -> Endpoint a { eFullConstructorName = b }
        SubApi   a -> SubApi a { feFullConstructorName = b }


shortConstructor :: Lens' Endpoint String
shortConstructor = lens getter setter
  where
    getter = \case
        Endpoint a -> eShortConstructor a
        SubApi   a -> feShortConstructor a

    setter s b = case s of
        Endpoint a -> Endpoint a { eShortConstructor = b }
        SubApi   a -> SubApi a { feShortConstructor = b }


handlerName :: Lens' Endpoint Name
handlerName = lens getter setter
  where
    getter = \case
        Endpoint a -> eHandlerName a
        SubApi   a -> feHandlerName a

    setter s b = case s of
        Endpoint a -> Endpoint a { eHandlerName = b }
        SubApi   a -> SubApi a { feHandlerName = b }


constructorArgs :: Lens' Endpoint [Type]
constructorArgs = lens getter setter
  where
    getter = \case
        Endpoint a -> eConstructorArgs a
        SubApi   a -> feConstructorArgs a

    setter s b = case s of
        Endpoint a -> Endpoint a { eConstructorArgs = b }
        SubApi   a -> SubApi a { feConstructorArgs = b }

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
    -- The normal case
    GadtC [name] bangArgs (AppT _ retType) -> do
        hRetType <- unitToNoContent retType
        let shortName = show $ unqualifiedName name
        pure . Endpoint $ EndpointData { eFullConstructorName = name
                                       , eShortConstructor    = shortName
                                       , eHandlerName = mkName $ lowerFirst shortName
                                       , eConstructorArgs     = fmap snd bangArgs
                                       , eConstructorReturns  = retType
                                       , eHandlerReturn       = hRetType
                                       }
    -- When the constructor contain references to other domain models
    ForallC [KindedTV var StarT] [] (GadtC [name] bangArgs (AppT _ _retType)) -> do
        let shortName = show $ unqualifiedName name
        -- [ ] Split the argument up into before and after the subcommand

        (args, subCmd) <- case reverse $ fmap snd bangArgs of
            AppT (ConT subCmd) (VarT var') : rest -> do
                unless (var == var')
                    $ error "Subcommand must be the last constructor argument"
                pure (reverse rest, subCmd)
            _ -> error $ "Last constructor argument must have form `SubCmd a`"

        let apiName    = mkName $ shortName <> "Api"
            serverName = mkName $ lowerFirst shortName <> "Server"
        pure . SubApi $ SubApiData { feFullConstructorName = name
                                   , feShortConstructor    = shortName
                                   , feHandlerName         = mkName $ lowerFirst shortName
                                   , feConstructorArgs     = args
                                   , feSubCmd              = subCmd
                                   , feSubApiName          = apiName
                                   , feSubServerName       = serverName
                                   }
    c ->
        error
            $  "Expected a GATD constructor representing an endpoint but got:\n"
            <> show c

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
mkServerSpec :: ServerType -> Name -> Q ServerSpec
mkServerSpec serverType n = do
    eps <- traverse toEndpoint =<< getConstructors =<< getCmdDec n
    let prefix = show $ unqualifiedName n
    pure ServerSpec { gadtName   = n
                    , apiName    = mkName $ prefix <> "Api"
                    , serverName = mkName $ lowerFirst prefix <> "Server"
                    , endpoints  = eps
                    , serverType = serverType
                    }

-- | Create type aliases for each endpoint, using constructor name prefixed with "Ep",
-- and a type `Api` that represents the full API.
mkCmdServer :: ServerOptions -> Name -> Q [Dec]
mkCmdServer opts = mkServerFromSpec opts <=< mkServerSpec CmdServer

mkQueryServer ::ServerOptions ->  Name -> Q [Dec]
mkQueryServer opts = mkServerFromSpec opts <=< mkServerSpec QueryServer

mkServerFromSpec :: ServerOptions -> ServerSpec -> Q [Dec]
mkServerFromSpec opts serverSpec = do
    subServers <- fmap mconcat . traverse (mkSubServer opts $ serverType serverSpec)
                $ endpoints serverSpec
    endpointDecs <- fmap mconcat . traverse (mkEndpointDec opts (serverType serverSpec)) $ endpoints serverSpec
    handlers     <- mkHandlers serverSpec
    server       <- mkFullServer serverSpec
    pure $ subServers <> endpointDecs <> handlers <> server

mkApiType :: [Endpoint] -> Q Type
mkApiType endpoints = case mkName . view shortConstructor <$> endpoints of
    []     -> error "Server has no endpoints"
    x : xs -> do
        let f :: Type -> Name -> Q Type
            f b a = [t| $(pure b) :<|> $(pure $ ConT a) |]
        foldM f (ConT x) xs

-- | Create a typealias for the API
-- type Api = EpA :<|> EpB :<|> EpC ...
mkApiDec :: ServerSpec -> Q Dec
mkApiDec spec =
    TySynD (apiName spec) [] <$> case mkName . view shortConstructor <$> endpoints spec of
        []     -> error "Server has no endpoints"
        x : xs -> do
            let f :: Type -> Name -> Q Type
                f b a = [t| $(pure b) :<|> $(pure $ ConT a) |]
            foldM f (ConT x) xs

-- | Create a request body by turning multiple arguments into a tuple
-- `BuyThing ItemKey Quantity` yields `ReqBody '[JSON] (ItemKey, Quantity)`
toReqBody :: [Type] -> Q (Maybe Type)
toReqBody args = mkBody
    >>= maybe (pure Nothing) (\b -> fmap Just [t| ReqBody '[JSON] $(pure b) |])
  where
    appAll :: Type -> [Type] -> Type
    appAll t = \case
        []     -> t
        x : xs -> appAll (AppT t x) xs

    mkBody :: Q (Maybe Type)
    mkBody = case args of
        []     -> pure Nothing
        ts     -> do
            let n = length ts
            pure . Just . AppT (ConT $ mkName "NamedFields") $ appAll (TupleT n) ts


-- | Define the servant endpoint type for non-hierarchical command constructors. E.g.
-- `BuyBook :: BookId -> Integer -> Cmd ()` will result in:
-- "BuyBook" :> ReqBody '[JSON] (BookId, Integer) -> Post '[JSON] NoContent
cmdEndpointType:: ServerOptions -> Endpoint -> Q Type
cmdEndpointType opts = \case
    Endpoint e -> epSimpleType e
    SubApi   e -> epSubApiType opts e
  where
    epSimpleType :: EndpointData -> Q Type
    epSimpleType e = [t| $(pure cmdName) :> $middle |]
      where
        cmdName :: Type
        cmdName = LitT . StrTyLit . renameConstructor opts $ eShortConstructor e

        middle :: Q Type
        middle = reqBody >>= \case
            Nothing -> reqReturn
            Just b  -> [t| $(pure b) :>  $reqReturn |]

        reqBody :: Q (Maybe Type)
        reqBody = toReqBody $ eConstructorArgs e

        reqReturn :: Q Type
        reqReturn = [t| Post '[JSON] $(pure $ eHandlerReturn e) |]

-- | Define the servant endpoint type for non-hierarchical query constructors. E.g.
-- `GetBook :: BookId -> Query Book` will result in:
-- "GetBook" :> Capture "BookId" BookId" :> Get '[JSON] Book
queryEndpointType:: ServerOptions -> Endpoint -> Q Type
queryEndpointType opts = \case
    Endpoint e -> epSimpleType e
    SubApi   e -> epSubApiType opts e
  where
    epSimpleType :: EndpointData -> Q Type
    epSimpleType e = [t| $(pure queryName) :> $(params $ eConstructorArgs e) |]
      where
        queryName :: Type
        queryName = LitT . StrTyLit  . renameConstructor opts $ eShortConstructor e

        params :: [Type] -> Q Type
        params typeList = do
            maybeType <- [t| Maybe |]
            case typeList of
                [] -> [t| Get '[JSON] $(pure $ eHandlerReturn e) |]
                AppT x t0:ts | x == maybeType -> do
                    tName <- case t0 of
                            ConT n -> pure n
                            VarT n -> pure n
                            err -> fail $ "Expected constructor parameter, got: "
                                        <> show err
                    let nameLit = LitT . StrTyLit . show $ unqualifiedName tName
                    appT (appT [t| (:>) |] [t| QueryParam $(pure nameLit) $(pure t0) |])
                         (params ts)
                t0:ts -> do
                    tName <- case t0 of
                            ConT n -> pure n
                            VarT n -> pure n
                            err -> fail $ "Expected constructor parameter, got: "
                                        <> show err
                    let nameLit = LitT . StrTyLit . show $ unqualifiedName tName
                    appT (appT [t| (:>) |] [t| Capture $(pure nameLit) $(pure t0) |])
                         (params ts)


-- | Define a servant endpoint ending in a reference to the sub API.
-- `EditBook :: BookId -> BookCmd a -> Cmd a` will result in
-- "EditBook" :> Capture "BookId" BookId :> BookApi
epSubApiType :: ServerOptions -> SubApiData -> Q Type
epSubApiType opts e = do

    let cmdName :: Type
        cmdName = LitT . StrTyLit . renameConstructor opts $ feShortConstructor e

        subApiType :: Type
        subApiType = ConT $ feSubApiName e

    bird <- [t| (:>) |]
    let mkCapture :: Type -> Q Type
        mkCapture t = do
            let pName = LitT . StrTyLit $ getTypeName t
            [t| Capture $(pure pName) $(pure t) |]

        getTypeName :: Type -> String
        getTypeName = \case
            ConT n -> show $ unqualifiedName n
            _ -> "typename"
    captures <- traverse mkCapture $ feConstructorArgs e
    pure $ foldr1 (\a b -> AppT (AppT bird a) b) (cmdName : captures <> [subApiType])

-- | Define a type alias representing the type of the endpoint
mkEndpointDec :: ServerOptions -> ServerType -> Endpoint -> Q [Dec]
mkEndpointDec opts sType e = do
    ty <- case sType of
        CmdServer -> cmdEndpointType opts e
        QueryServer -> queryEndpointType opts e
    pure $ [TySynD (mkName $ view shortConstructor e) [] ty]

mkSubServer :: ServerOptions -> ServerType -> Endpoint -> Q [Dec]
mkSubServer opts sTy = \case
    Endpoint _ -> pure []
    SubApi e -> do
        subServerSpec <-
            set (field @"apiName") (feSubApiName e)
            .   set (field @"serverName") (feSubServerName e)
            <$> mkServerSpec sTy (feSubCmd e)
        subServerDecs <- mkServerFromSpec opts subServerSpec
        pure subServerDecs


-- | Make command handlers for each endpoint
mkHandlers :: ServerSpec -> Q [Dec]
mkHandlers spec = fmap mconcat . forM (endpoints spec) $ \ep -> do
    runnerTy <- runnerType spec
    case (ep, serverType spec) of
        (Endpoint e, CmdServer) -> mkCmdEPHandler runnerTy e
        (Endpoint e, QueryServer) -> mkQueryEPHandler runnerTy e
        (SubApi   e, _) -> mkSubAPiHandler runnerTy e

mkQueryEPHandler :: Type -> EndpointData -> Q [Dec]
mkQueryEPHandler runnerTy e = do
    varNames       <- traverse (const $ newName "arg") $ eConstructorArgs e
    handlerRetType <- [t| Handler $(pure $ eHandlerReturn e) |]
    let varPat =  fmap VarP varNames
        nrArgs = length @[] $ eConstructorArgs e
        funSig =
            SigD (eHandlerName e)
                . AppT (AppT ArrowT runnerTy)
                $ case eConstructorArgs e of
                      []  -> handlerRetType
                      ts -> foldr (AppT . AppT ArrowT) handlerRetType ts
        funBodyBase = AppE (VarE runner)
            $ foldl AppE (ConE $ eFullConstructorName e) (fmap VarE varNames)

        funBody = case eConstructorReturns e of
            TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
            _        -> pure $ funBodyBase
    funClause <- clause
        (pure (VarP runner) : (if nrArgs > 0 then (fmap pure varPat) else []))
        (normalB [| liftIO $ $(funBody)  |])
        []
    let funDef = FunD (eHandlerName e) [funClause]
    pure [funSig, funDef]
  where
    runner :: Name
    runner = mkName "runner"

mkCmdEPHandler :: Type -> EndpointData -> Q [Dec]
mkCmdEPHandler runnerTy e = do
    varNames       <- traverse (const $ newName "arg") $ eConstructorArgs e
    handlerRetType <- [t| Handler $(pure $ eHandlerReturn e) |]
    let varPat =  ConP (mkName "NamedFields") [TupP $ fmap VarP varNames]
        nrArgs = length @[] $ eConstructorArgs e
        funSig =
            SigD (eHandlerName e)
                . AppT (AppT ArrowT runnerTy)
                $ case eConstructorArgs e of
                      []  -> handlerRetType
                      as  -> AppT (AppT ArrowT (AppT (ConT $ mkName "NamedFields")
                                $ foldl AppT (TupleT (length as)) as))
                                  handlerRetType

        funBodyBase = AppE (VarE runner)
            $ foldl AppE (ConE $ eFullConstructorName e) (fmap VarE varNames)

        funBody = case eConstructorReturns e of
            TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
            _        -> pure $ funBodyBase
    funClause <- clause
        (pure (VarP runner) : (if nrArgs > 0 then [pure $ varPat] else []))
        (normalB [| liftIO $ $(funBody)  |])
        []
    let funDef = FunD (eHandlerName e) [funClause]
    pure [funSig, funDef]
  where
    runner :: Name
    runner = mkName "runner"

mkSubAPiHandler :: Type -> SubApiData -> Q [Dec]
mkSubAPiHandler runnerTy e = do
    runner <- newName "runner"

    paramNames  <- traverse (const $ newName "arg") $ feConstructorArgs e
    let finalSig = [t| Server $(pure . ConT $ feSubApiName e) |]
    finalSig' <- finalSig
    params <- case feConstructorArgs e of
        [] -> [t| $(pure runnerTy) -> $finalSig |]
        [a] -> [t| $(pure runnerTy) -> $(pure a) -> $finalSig |]
        ts -> pure $ foldr1 (\b a -> AppT (AppT ArrowT b) a)
                    (runnerTy : ts <> [finalSig'])
    funSig <- SigD (feHandlerName e) <$> pure params

    funClause <- case fmap VarE paramNames of
        [] -> clause
                [varP runner]
                (fmap NormalB
                      [e| $(varE $ feSubServerName e)
                             $ $(pure $ VarE runner)
                             . $(pure . ConE . mkName $ feShortConstructor e) |]
                )
                []
        ts ->
            let cmd = foldl (\b a -> AppE b a) (ConE . mkName $ feShortConstructor e) ts
             in clause
                  (varP <$>  runner : paramNames)
                  (fmap NormalB
                        [e| $(varE $ feSubServerName e)
                                $ $(varE runner)
                                . $(pure cmd)
                        |]
                  )
                  []
    let funDef = FunD (feHandlerName e) [funClause]
    pure [funSig, funDef]

-- | The type of the CmdRunner used to execute commands
runnerType :: ServerSpec -> Q Type
runnerType spec = case serverType spec of
    QueryServer -> [t| QueryRunner $(pure . ConT $ gadtName spec) |]
    CmdServer -> [t| CmdRunner $(pure . ConT $ gadtName spec) |]

-- | Create the full server and api dec
-- Assumes the handlers have already been created (using `mkHandlers`)
mkFullServer :: ServerSpec -> Q [Dec]
mkFullServer spec = do
    runnerT <- runnerType spec
    let runner = mkName "runner"

        handlerExp :: Endpoint -> Exp
        handlerExp e = VarE (e ^. handlerName) `AppE` VarE runner

    body <- case handlerExp <$> endpoints spec of
        []     -> error "Server contains no endpoints"
        e : es -> foldM (\b a -> [| $(pure b) :<|> $(pure a) |]) e es
    apiDec        <- mkApiDec spec
    serverTypeDec <-
        SigD (serverName spec)
        .   AppT (AppT ArrowT runnerT)
        <$> [t| Server $(pure $ ConT $ apiName spec) |]
    let funDec = FunD (serverName spec) [Clause [VarP runner] (NormalB body) []]
    pure [apiDec, serverTypeDec, funDec]
