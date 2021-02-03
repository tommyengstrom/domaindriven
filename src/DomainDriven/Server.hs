{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DomainDriven.Server
    ( module DomainDriven.Server
    , liftIO
    , module DomainDriven.Internal.NamedFields
    , HasFieldName(..)
    ) where

import           DomainDriven.Internal.Class
import           DomainDriven.Internal.HasFieldName
                                                ( HasFieldName(..) )
import           Prelude
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     ( OccName(..) )
import           Control.Monad
import qualified Data.List                                    as L
import           Servant
import           Data.Char
import           Control.Monad.Trans
import           Control.Lens
import           Data.Generics.Product
import           GHC.Generics                   ( Generic )
import           DomainDriven.Internal.NamedFields

data ServerSpec = ServerSpec
    { gadtName   :: Name -- ^ Name of the GADT representing the command
    , apiName    :: Name
    , serverName :: Name
    , endpoints  :: [ApiPiece] -- ^ Endpoints created from the constructors of the GADT
    , serverType :: ServerType
    }
    deriving (Show, Generic)

data ServerType
    = CmdServer -- ^ A server that runs commands, i.e. requires a command runner to run
    | QueryServer -- ^ A server that runs queries.
    deriving (Show, Generic)

data ApiPiece
    = Endpoint EndpointData
    | SubApi SubApiData
    deriving (Show, Generic)


data EndpointData = EndpointData
    { constructorName   :: Name
    , pathPrefix        :: [String]
    , apiTypeName       :: Name
    , handlerName       :: Name
    , constructorArgs   :: [Type]
    , returnType        :: Type
    , handlerReturnType :: Type -- ^ Same as for constructor but with NoContent instead of ()
    }
    deriving (Show, Generic)

data SubApiData = SubApiData
    { constructorName   :: Name
    , pathPrefix        :: [String]
    , apiTypeName       :: Name
    , handlerName       :: Name
    , constructorArgs   :: [Type]
    , subCmd            :: Name
    , subCmdApiTypeName :: Name
    , subCmdHandlerName :: Name
    }
    deriving (Show, Generic)

data ServerOptions = ServerOptions
    { renameConstructor :: String -> [String]
    , prefix            :: String
    , unitIsNoContent   :: Bool -- ^ Translate () in commands to NoContent endpoints
                                -- (Aeson encodes unit at "[]")
    }
    deriving Generic

defaultServerOptions :: ServerOptions
defaultServerOptions =
    ServerOptions { renameConstructor = pure, prefix = "", unitIsNoContent = True }

epName :: Lens' ApiPiece Name
epName = lens getter setter
  where
    getter = \case
        Endpoint a -> a ^. field @"apiTypeName"
        SubApi   a -> a ^. field @"apiTypeName"

    setter s b = case s of
        Endpoint a -> Endpoint a { apiTypeName = b }
        SubApi   a -> SubApi a { apiTypeName = b }

epPathPrefix :: Lens' ApiPiece [String]
epPathPrefix = lens getter setter
  where
    getter = \case
        Endpoint a -> a ^. field @"pathPrefix"
        SubApi   a -> a ^. field @"pathPrefix"

    setter s b = case s of
        Endpoint a -> Endpoint a { pathPrefix = b }
        SubApi   a -> SubApi a { pathPrefix = b }


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

unqualified :: Lens' Name String
unqualified = typed @OccName . typed

type Prefix = String

toEndpoint :: ServerOptions -> Con -> Q ApiPiece
toEndpoint opts = \case
    -- The normal case
    GadtC [name] bangArgs (AppT _ retType) -> do
        hRetType <- case retType of
                                      TupleT 0 | opts ^. field @"unitIsNoContent" ->
                                          [t| NoContent|]
                                      t -> pure t

        let shortName = name ^. unqualified
        pure . Endpoint $ EndpointData
            { constructorName   = name
            , pathPrefix        = renameConstructor opts shortName
            , apiTypeName   = mkName $ prefix opts <> shortName
            , handlerName       = mkName $ lowerFirst $ prefix opts <> shortName
            , constructorArgs   = fmap snd bangArgs
            , returnType        = retType
            , handlerReturnType = hRetType
            }
    -- When the constructor contain references to other domain models
    ForallC [KindedTV var StarT] [] (GadtC [name] bangArgs (AppT _ _retType)) -> do
        let shortName = name ^. unqualified
        -- [ ] Split the argument up into before and after the subcommand

        (args, subCmd') <- case reverse $ fmap snd bangArgs of
            AppT (ConT subCmd') (VarT var') : rest -> do
                unless (var == var')
                    $ error "Subcommand must be the last constructor argument"
                pure (reverse rest, subCmd')
            _ -> error $ "Last constructor argument must have form `SubCmd a`"

        let serverName = mkName $ lowerFirst $ prefix opts <> shortName <> "Server"
        pure . SubApi $ SubApiData
            { constructorName = name
            , pathPrefix      = renameConstructor opts shortName
            , apiTypeName = mkName $ prefix opts <> shortName
            , handlerName     = mkName $ lowerFirst $ prefix opts <> shortName
            , constructorArgs = args
            , subCmd        = subCmd'
            , subCmdApiTypeName    = mkName $ prefix opts <> shortName <> "Api"
            , subCmdHandlerName = serverName
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

upperFirst :: String -> String
upperFirst = \case
    c : cs -> toUpper c : cs
    []     -> []


-- | Create a ServerSpec from a GADT
-- The GADT must have one parameter representing the return type
mkServerSpec :: ServerOptions -> ServerType -> Name -> Q ServerSpec
mkServerSpec opts' serverType n = do
    let opts = opts' & field @"prefix" %~ (upperFirst . (<> n ^. unqualified))
    eps <- traverse (toEndpoint opts) =<< getConstructors =<< getCmdDec n

    ensureUniquePaths eps
    pure ServerSpec { gadtName   = n
                    , apiName    = mkName $ prefix opts <> "Api"
                    , serverName = mkName $ lowerFirst (prefix opts <> "Server")
                    , endpoints  = eps
                    , serverType = serverType
                    }
  where
    ensureUniquePaths :: [ApiPiece] -> Q ()
    ensureUniquePaths eps = do

        let paths :: [[String]]
            paths       = eps ^.. folded . epPathPrefix

            uniquePaths = L.nub paths
            duplicates  = L.foldl' (flip L.delete) paths uniquePaths

        unless
            (null duplicates)
            (fail $ "Api contains duplicated paths:\n * " <> L.intercalate
                "\n * "
                (show . L.intercalate "/" <$> duplicates)
            )

-- | Create type aliases for each endpoint, using constructor name prefixed with "Ep",
-- and a type `Api` that represents the full API.
mkCmdServer :: ServerOptions -> Name -> Q [Dec]
mkCmdServer opts = mkServerFromSpec opts <=< mkServerSpec opts CmdServer

mkQueryServer :: ServerOptions -> Name -> Q [Dec]
mkQueryServer opts = mkServerFromSpec opts <=< mkServerSpec opts QueryServer

mkServerFromSpec :: ServerOptions -> ServerSpec -> Q [Dec]
mkServerFromSpec opts serverSpec = do
    subServers <-
        fmap mconcat . traverse (mkSubServer opts $ serverType serverSpec) $ endpoints
            serverSpec
    endpointDecs <-
        fmap mconcat . traverse (mkEndpointDec (serverType serverSpec)) $ endpoints
            serverSpec
    handlers <- mkHandlers serverSpec
    server   <- mkFullServer serverSpec
    pure $ subServers <> endpointDecs <> handlers <> server

mkApiType :: [ApiPiece] -> Q Type
mkApiType endpoints = case view epName <$> endpoints of
    []     -> error "Server has no endpoints"
    x : xs -> do
        let f :: Type -> Name -> Q Type
            f b a = [t| $(pure b) :<|> $(pure $ ConT a) |]
        foldM f (ConT x) xs

-- | Create a typealias for the API
-- type Api = EpA :<|> EpB :<|> EpC ...
mkApiDec :: ServerSpec -> Q Dec
mkApiDec spec = TySynD (apiName spec) [] <$> case view epName <$> endpoints spec of
    []     -> error "Server has no endpoints"
    x : xs -> do
        let f :: Type -> Name -> Q Type
            f b a = [t| $(pure b) :<|> $(pure $ ConT a) |]
        foldM f (ConT x) xs

-- | Create a request body by turning multiple arguments into a NamedFieldsN
-- `BuyThing ItemKey Quantity`
-- yields:
-- `ReqBody '[JSON] (NamedFields2 "BuyThing" ItemKey Quantity)`
toReqBody :: EndpointData -> [Type] -> Q (Maybe Type)
toReqBody e args = do
    unless
        (args == L.nub args)
        (fail
        $ "Each argument to the constructor must have a unique type.\
           \\nGot request body containing: "
        <> L.intercalate ", " (args ^.. folded . types @Name . to show)
        )
    mkBody >>= maybe (pure Nothing) (\b -> fmap Just [t| ReqBody '[JSON] $(pure b) |])
  where
    mkBody :: Q (Maybe Type)
    mkBody = case args of
        [] -> pure Nothing
        ts -> do
            let n           = length ts
                constructor = AppT
                    (ConT (mkName $ "NamedFields" <> show n))
                    (LitT . StrTyLit $ e ^. field @"apiTypeName" . to show . to
                        (<> "Body")
                    )
            pure . Just $ foldl AppT constructor ts

-- | Define the servant endpoint type for non-hierarchical command constructors. E.g.
-- `BuyBook :: BookId -> Integer -> Cmd ()` will result in:
-- "BuyBook" :> ReqBody '[JSON] (BookId, Integer) -> Post '[JSON] NoContent
cmdEndpointType :: ApiPiece -> Q Type
cmdEndpointType = \case
    Endpoint e -> epSimpleType e
    SubApi   e -> epSubApiType e
  where
    epSimpleType :: EndpointData -> Q Type
    epSimpleType e =
        [t| $(mkServantEpName (e ^. field @"pathPrefix") middle) |]
      where
        middle :: Q Type
        middle = reqBody >>= \case
            Nothing -> reqReturn
            Just b  -> [t| $(pure b) :>  $reqReturn |]

        reqBody :: Q (Maybe Type)
        reqBody = toReqBody e $ e ^. field @"constructorArgs"

        reqReturn :: Q Type
        reqReturn = [t| Post '[JSON] $(pure $ e ^. field @"handlerReturnType") |]

-- | Define the servant endpoint type for non-hierarchical query constructors. E.g.
-- `GetBook :: BookId -> Query Book` will result in:
-- "GetBook" :> Capture "BookId" BookId" :> Get '[JSON] Book
queryEndpointType :: ApiPiece -> Q Type
queryEndpointType = \case
    Endpoint e -> epSimpleType e
    SubApi   e -> epSubApiType e
  where
    epSimpleType :: EndpointData -> Q Type
    epSimpleType e =
        [t| $(mkServantEpName (e ^. field @"pathPrefix") (params $ e ^. field @"constructorArgs")) |]
      where

        params :: [Type] -> Q Type
        params typeList = do
            maybeType <- [t| Maybe |]
            case typeList of
                [] -> [t| Get '[JSON] $(pure $ e ^. field @"handlerReturnType") |]
                AppT x t0 : ts | x == maybeType -> do
                    tName <- case t0 of
                        ConT n -> pure n
                        VarT n -> pure n
                        err -> fail $ "Expected constructor parameter, got: " <> show err
                    let nameLit = LitT . StrTyLit $ tName ^. unqualified
                    appT
                        (appT [t| (:>) |] [t| QueryParam $(pure nameLit) $(pure t0) |])
                        (params ts)
                t0 : ts -> do
                    tName <- case t0 of
                        ConT n -> pure n
                        VarT n -> pure n
                        err -> fail $ "Expected constructor parameter, got: " <> show err
                    let nameLit = LitT . StrTyLit $ tName ^. unqualified
                    appT (appT [t| (:>) |] [t| Capture $(pure nameLit) $(pure t0) |])
                         (params ts)


mkServantEpName :: [String] -> Q Type -> Q Type
mkServantEpName tyLits rest =
    foldr (\a b -> [t| $(pure a) :> $b |]) rest (fmap (LitT . StrTyLit) tyLits)

-- | Define a servant endpoint ending in a reference to the sub API.
-- `EditBook :: BookId -> BookCmd a -> Cmd a` will result in
-- "EditBook" :> Capture "BookId" BookId :> BookApi
epSubApiType :: SubApiData -> Q Type
epSubApiType e = do

    let subApiType :: Type
        subApiType = ConT $ subCmdApiTypeName e

    bird <- [t| (:>) |]
    let mkCapture :: Type -> Q Type
        mkCapture t = do
            let pName = LitT . StrTyLit $ getTypeName t
            [t| Capture $(pure pName) $(pure t) |]

        getTypeName :: Type -> String
        getTypeName = \case
            ConT n -> n ^. unqualified
            _      -> "typename"
    captures <- traverse mkCapture $ e ^. field @"constructorArgs"
    mkServantEpName
        (e ^. field @"pathPrefix")
        (pure $ foldr1 (\a b -> AppT (AppT bird a) b) (captures <> [subApiType]))

-- | Define a type alias representing the type of the endpoint
mkEndpointDec :: ServerType -> ApiPiece -> Q [Dec]
mkEndpointDec sType e = do
    ty <- case sType of
        CmdServer   -> cmdEndpointType e
        QueryServer -> queryEndpointType e
    pure $ [TySynD (view epName e) [] ty]

mkSubServer :: ServerOptions -> ServerType -> ApiPiece -> Q [Dec]
mkSubServer opts' sTy = \case
    Endpoint _ -> pure []
    SubApi   e -> do
        let
            opts =
                opts'
                    &  field @"prefix"
                    %~ (<> e ^. field @"constructorName" . unqualified)
        subServerSpec <-
            set (field @"apiName") (subCmdApiTypeName e)
            .   set (field @"serverName") (subCmdHandlerName e)
            <$> mkServerSpec opts sTy (subCmd e)
        subServerDecs <- mkServerFromSpec opts subServerSpec
        pure subServerDecs


-- | Make command handlers for each endpoint
mkHandlers :: ServerSpec -> Q [Dec]
mkHandlers spec = fmap mconcat . forM (endpoints spec) $ \ep -> do
    runnerTy <- runnerType spec
    case (ep, serverType spec) of
        (Endpoint e, CmdServer  ) -> mkCmdEPHandler runnerTy e
        (Endpoint e, QueryServer) -> mkQueryEPHandler runnerTy e
        (SubApi   e, _          ) -> mkSubAPiHandler runnerTy e

mkQueryEPHandler :: Type -> EndpointData -> Q [Dec]
mkQueryEPHandler runnerTy e = do
    varNames       <- traverse (const $ newName "arg") $ e ^. field @"constructorArgs"
    handlerRetType <- [t| Handler $(pure $ e ^. field @"handlerReturnType") |]
    let varPat = fmap VarP varNames
        nrArgs = length @[] $ e ^. field @"constructorArgs"
        funSig =
            SigD (e ^. field @"handlerName")
                . AppT (AppT ArrowT runnerTy)
                $ case e ^. field @"constructorArgs" of
                      [] -> handlerRetType
                      ts -> foldr (AppT . AppT ArrowT) handlerRetType ts
        funBodyBase = AppE (VarE runner)
            $ foldl AppE (ConE $ e ^. field @"constructorName") (fmap VarE varNames)

        funBody = case e ^. field @"returnType" of
            TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
            _        -> pure $ funBodyBase
    funClause <- clause
        (pure (VarP runner) : (if nrArgs > 0 then (fmap pure varPat) else []))
        (normalB [| liftIO $ $(funBody)  |])
        []
    let funDef = FunD (e ^. field @"handlerName") [funClause]
    pure [funSig, funDef]
  where
    runner :: Name
    runner = mkName "runner"

mkCmdEPHandler :: Type -> EndpointData -> Q [Dec]
mkCmdEPHandler runnerTy e = do
    varNames       <- traverse (const $ newName "arg") $ e ^. field @"constructorArgs"
    handlerRetType <- [t| Handler $(pure $ e ^. field @"handlerReturnType") |]
    let varPat          = ConP constructorName (fmap VarP varNames)
        constructorName = mkName $ "NamedFields" <> show (length varNames)
        nrArgs          = length @[] $ e ^. field @"constructorArgs"
        funSig =
            SigD (e ^. field @"handlerName")
                . AppT (AppT ArrowT runnerTy)
                $ case e ^. field @"constructorArgs" of
                      [] -> handlerRetType
                      as ->
                          let
                              nfType :: Type
                              nfType = AppT
                                  (ConT constructorName)
                                  (  LitT
                                  .  StrTyLit
                                  $  e
                                  ^. field @"apiTypeName"
                                  .  to show
                                  .  to (<> "Body")
                                  )
                          in
                              AppT (AppT ArrowT (foldl AppT nfType as)) handlerRetType

        funBodyBase = AppE (VarE runner)
            $ foldl AppE (ConE $ e ^. field @"constructorName") (fmap VarE varNames)

        funBody = case e ^. field @"returnType" of
            TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
            _        -> pure $ funBodyBase
    funClause <- clause
        (pure (VarP runner) : (if nrArgs > 0 then [pure $ varPat] else []))
        (normalB [| liftIO $ $(funBody)  |])
        []
    let funDef = FunD (e ^. field @"handlerName") [funClause]
    pure [funSig, funDef]
  where
    runner :: Name
    runner = mkName "runner"

mkSubAPiHandler :: Type -> SubApiData -> Q [Dec]
mkSubAPiHandler runnerTy e = do
    runner <- newName "runner"

    paramNames  <- traverse (const $ newName "arg") $ e ^. field @"constructorArgs"
    let finalSig = [t| Server $(pure . ConT $ subCmdApiTypeName e) |]
    finalSig' <- finalSig
    params <- case e ^. field @"constructorArgs" of
        [] -> [t| $(pure runnerTy) -> $finalSig |]
        [a] -> [t| $(pure runnerTy) -> $(pure a) -> $finalSig |]
        ts -> pure $ foldr1 (\b a -> AppT (AppT ArrowT b) a)
                    (runnerTy : ts <> [finalSig'])
    funSig <- SigD (e ^. field @"handlerName") <$> pure params

    funClause <- case fmap VarE paramNames of
        [] -> clause
                [varP runner]
                (fmap NormalB
                      [e| $(varE $ subCmdHandlerName e)
                             $ $(pure $ VarE runner)
                             . $(pure . ConE $ e ^. field @"constructorName") |]
                )
                []
        ts ->
            let cmd = foldl (\b a -> AppE b a) (ConE $ e ^. field @"constructorName") ts
             in clause
                  (varP <$>  runner : paramNames)
                  (fmap NormalB
                        [e| $(varE $ subCmdHandlerName e)
                                $ $(varE runner)
                                . $(pure cmd)
                        |]
                  )
                  []
    let funDef = FunD (e ^. field @"handlerName") [funClause]
    pure [funSig, funDef]

-- | The type of the CmdRunner used to execute commands
runnerType :: ServerSpec -> Q Type
runnerType spec = case serverType spec of
    QueryServer -> [t| QueryRunner $(pure . ConT $ gadtName spec) |]
    CmdServer   -> [t| CmdRunner $(pure . ConT $ gadtName spec) |]

-- | Create the full server and api dec
-- Assumes the handlers have already been created (using `mkHandlers`)
mkFullServer :: ServerSpec -> Q [Dec]
mkFullServer spec = do
    runnerT <- runnerType spec
    let runner = mkName "runner"

        handlerExp :: ApiPiece -> Exp
        handlerExp = \case
            Endpoint a -> VarE (a ^. field @"handlerName") `AppE` VarE runner
            SubApi   a -> VarE (a ^. field @"handlerName") `AppE` VarE runner

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
