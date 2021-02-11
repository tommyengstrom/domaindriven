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
import           Data.Generics.Sum
import           GHC.Generics                   ( Generic )
import           DomainDriven.Internal.NamedFields
import           Control.Monad.Reader

data ApiSpec = ApiSpec
    { gadtName   :: Name -- ^ Name of the GADT representing the command
    , urlPieces  :: [UrlSegment] -- ^ The Url path segments this API adds
    , endpoints  :: [ApiPiece] -- ^ Endpoints created from the constructors of the GADT
    , serverType :: ServerType
    }
    deriving (Show, Generic)

qualifiedName :: Monad m => Name -> ReaderT [Name] m Name
qualifiedName n = do
    s <- ask
    pure . mkName . mconcat . fmap show $ s <> [n]

askApiTypeName :: Monad m => Name -> ReaderT [Name] m Name
askApiTypeName n = (unqualifiedString <>~ "Api") <$> qualifiedName n

askServerName :: Monad m => Name -> ReaderT [Name] m Name
askServerName n =
    (unqualifiedString <>~ "Server")
        .   (unqualifiedString %~ lowerFirst)
        <$> qualifiedName n

askHandlerName :: Monad m => Name -> ReaderT [Name] m Name
askHandlerName n =
    (unqualifiedString <>~ "Handler")
        .   (unqualifiedString %~ lowerFirst)
        <$> qualifiedName n

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- I need to create a structure that contians both Name and [UrlSegment] to use for the
-- reader. BodyTag should have the "exposed name" but the rest of the stuff should have
-- the normal names without any mapping.
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
askBodyTag :: Monad m => ReaderT [Name] m TyLit
askBodyTag = do
    s <- qualifiedName
    pure . StrTyLit . show $ s & unqualifiedString <>~ "Body"


askApiPieceTypeName :: Monad m => ApiPiece -> ReaderT [Name] m Name
askApiPieceTypeName = \case
    Endpoint e -> local (<> e ^. typed) askApiTypeName
    SubApi   e -> local (<> e ^. typed) askApiTypeName

newtype UrlSegment = UrlSegment String
    deriving (Show, Generic, Eq)

mkUrlSegments :: ServerOptions -> Name -> [UrlSegment]
mkUrlSegments opts n =
    n ^.. unqualifiedString . to (renameConstructor opts) . folded . to UrlSegment

instance {-# Overlapping #-} HasType [UrlSegment] ApiPiece where
    setTyped a = \case
        Endpoint e -> Endpoint $ e & typed .~ a
        SubApi   e -> SubApi $ e & typed .~ a
    getTyped = \case
        Endpoint e -> e ^. typed
        SubApi   e -> e ^. typed

data ServerType
    = CmdServer -- ^ A server that runs commands, i.e. requires a command runner to run
    | QueryServer -- ^ A server that runs queries.
    deriving (Show, Generic)

data ApiPiece
    = Endpoint EndpointData
    | SubApi SubApiData
    deriving (Show, Generic)


data EndpointData = EndpointData
    { gadtName          :: Name
    , urlPieces         :: [UrlSegment]
    , constructorArgs   :: [Type]
    , returnType        :: Type
    , handlerReturnType :: Type -- ^ Same as for constructor but with NoContent instead of ()
    }
    deriving (Show, Generic)

data SubApiData = SubApiData
    { gadtName        :: Name
    , urlPieces       :: [UrlSegment]
    , constructorArgs :: [Type]
    , subApi          :: ApiSpec
    }
    deriving (Show, Generic)

data ServerOptions = ServerOptions
    { renameConstructor :: String -> [String]
    , unitIsNoContent   :: Bool -- ^ Translate () in commands to NoContent endpoints
                                -- (Aeson encodes unit at "[]")
    }
    deriving Generic

defaultServerOptions :: ServerOptions
defaultServerOptions = ServerOptions { renameConstructor = pure, unitIsNoContent = True }

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

unqualifiedString :: Lens' Name String
unqualifiedString = typed @OccName . typed

toEndpoint :: ServerOptions -> ServerType -> Con -> Q ApiPiece
toEndpoint opts serverType = \case
    -- The normal case
    GadtC [gadtName] bangArgs (AppT _ retType) -> do
        hRetType <- case retType of
                                      TupleT 0 | opts ^. field @"unitIsNoContent" ->
                                          [t| NoContent|]
                                      t -> pure t

        pure . Endpoint $ EndpointData
            { gadtName   = gadtName
            , urlPieces    = mkUrlSegments opts gadtName
            , constructorArgs   = fmap snd bangArgs
            , returnType        = retType
            , handlerReturnType = hRetType
            }
    -- When the constructor contain references to other domain models
    ForallC [KindedTV var StarT] [] (GadtC [gadtName] bangArgs (AppT _ _retType)) -> do
        (args, subCmd') <- case reverse $ fmap snd bangArgs of
            AppT (ConT subCmd') (VarT var') : rest -> do
                unless (var == var')
                    $ error "Subcommand must be the last constructor argument"
                pure (reverse rest, subCmd')
            _ -> error $ "Last constructor argument must have form `SubCmd a`"

        subServerSpec <- mkServerSpec opts serverType subCmd'
        pure . SubApi $ SubApiData
            { gadtName   = gadtName
            , urlPieces    = mkUrlSegments opts gadtName
            , constructorArgs   = args
            , subApi = subServerSpec
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


-- | Create a ApiSpec from a GADT
-- The GADT must have one parameter representing the return type
mkServerSpec :: ServerOptions -> ServerType -> Name -> Q ApiSpec
mkServerSpec opts serverType n = do
    eps <- traverse (toEndpoint opts serverType) =<< getConstructors =<< getCmdDec n

    ensureUniquePaths eps
    pure ApiSpec { gadtName   = n
                 , urlPieces  = mkUrlSegments opts n
                 , endpoints  = eps
                 , serverType = serverType
                 }
  where
    -- FIXME: Improve error message to show the full gadtName of offending types and
    -- constructors
    ensureUniquePaths :: [ApiPiece] -> Q ()
    ensureUniquePaths eps = do



        let paths :: [(Name, String)]
            paths      = L.sortOn snd $ foldMap (fullConstructorPaths opts) eps

            duplicates = L.nub $ mconcat $ zipWith
                (\a b -> if snd a == snd b then [a, b] else [])
                paths
                (drop 1 paths)

        unless
            (null duplicates)
            (fail $ "Api contains duplicated paths:\n * " <> L.intercalate
                "\n * "
                (show <$> duplicates)
            )

fullConstructorPaths :: ServerOptions -> ApiPiece -> [(Name, String)]
fullConstructorPaths opts = \case
    Endpoint a ->
        [ ( a ^. typed
          , L.intercalate "/" $ a ^. typed @[UrlSegment] & traversed %~ view typed
          )
        ]
    SubApi a -> mconcat $ a ^.. typed @ApiSpec . typed @[ApiPiece] . folded . to
        (fullConstructorPaths opts)

-- | Create type aliases for each endpoint, using constructor gadtName prefixed with "Ep",
-- and a type `Api` that represents the full API.
mkCmdServer :: ServerOptions -> Name -> Q [Dec]
mkCmdServer opts gadtName = do
    spec <- mkServerSpec opts CmdServer gadtName
    runReaderT (mkServerFromSpec opts spec) []

mkQueryServer :: ServerOptions -> Name -> Q [Dec]
mkQueryServer opts gadtName = do
    spec <- mkServerSpec opts QueryServer gadtName
    runReaderT (mkServerFromSpec opts spec) []

-- | This is the only layer of the ReaderT stack where we do not use `local` to update the
-- url segments.
mkServerFromSpec :: ServerOptions -> ApiSpec -> ReaderT [Name] Q [Dec]
mkServerFromSpec opts s = do
    subServers <- local (<> s ^. typed) $ do
        let subApis :: [ApiSpec]
            subApis = s ^.. typed @[ApiPiece] . folded . _Typed @SubApiData . typed
        mconcat <$> traverse (mkServerFromSpec opts) subApis

    endpointDecs <- mkEndpoints s
    handlers     <- mkHandlers s
    server       <- mkFullServer s
    pure $ subServers <> endpointDecs <> handlers <> server

bogusD :: String -> Dec
bogusD s = FunD (mkName s) [Clause [] (NormalB $ VarE $ mkName s) []]
--
-- | Create a typealias for the API
-- type Api = EpA :<|> EpB :<|> EpC ...
mkApiDec :: Name -> [Name] -> Q Dec
mkApiDec apiName apiPieceNames = do
    TySynD apiName [] <$> case apiPieceNames of
        []     -> error "Server has no endpoints"
        x : xs -> do
            foldM f (ConT x) xs
  where
    f :: Type -> Name -> Q Type
    f b a = [t| $(pure b) :<|> $(pure $ ConT a) |]
-- | Create a request body by turning multiple arguments into a NamedFieldsN
-- `BuyThing ItemKey Quantity`
-- yields:
-- `ReqBody '[JSON] (NamedFields2 "BuyThing" ItemKey Quantity)`
mkReqBody :: TyLit -> [Type] -> Q (Maybe Type)
mkReqBody bodyTag args = do
    unless
        (args == L.nub args)
        (fail
        $ "Each argument to the constructor must have a unique type.\
           \\nGot request body containing: "
        <> L.intercalate ", " (args ^.. folded . types @Name . to show)
        )
    let body = case args of
            [] -> Nothing
            ts ->
                let n = length ts
                    constructor =
                        AppT (ConT (mkName $ "NamedFields" <> show n)) (LitT bodyTag)
                in  Just $ foldl AppT constructor ts
    case body of
        Nothing -> pure Nothing
        Just b  -> Just <$> [t| ReqBody '[JSON] $(pure b) |]


-- | Tag used as the symbol in in `NamedFieldsN`.
-- This field is used to gadtName the type in the OpenAPI definition.
--mkBodyTag :: EndpointData -> String
--mkBodyTag e = mconcat . (<> ["Body"]) $ prefixes & traversed . _head %~ toUpper
--  where
--    prefixes :: [String]
--    prefixes = e ^.. field @"urlPieces" . folded . to show

-- | Define the servant endpoint type for non-hierarchical command constructors. E.g.
-- `BuyBook :: BookId -> Integer -> Cmd ()` will result in:
-- "BuyBook" :> ReqBody '[JSON] (BookId, Integer) -> Post '[JSON] NoContent
cmdEndpointType :: ApiPiece -> ReaderT [Name] Q Type
cmdEndpointType p = case p of
    SubApi   e -> epSubApiType e
    Endpoint e -> local (<> p ^. typed) $ do
        bodyTag <- askBodyTag
        reqBody <- lift $ mkReqBody bodyTag (e ^. typed)
        middle  <- case reqBody of
            Nothing -> lift reqReturn
            Just b  -> lift [t| $(pure b) :>  $reqReturn |]
        lift $ prependServerEndpointName (e ^. typed) middle
      where
        reqReturn :: Q Type
        reqReturn = [t| Post '[JSON] $(pure $ e ^. field @"handlerReturnType") |]

-- | Define the servant endpoint type for non-hierarchical query constructors. E.g.
-- `GetBook :: BookId -> Query Book` will result in:
-- "GetBook" :> Capture "BookId" BookId" :> Get '[JSON] Book
queryEndpointType :: ApiPiece -> ReaderT [Name] Q Type
queryEndpointType p = case p of
    SubApi   e -> epSubApiType e
    Endpoint e -> local (<> p ^. typed) $ do
        ty <- lift . params $ e ^. field @"constructorArgs"
        lift $ prependServerEndpointName (e ^. typed) ty
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
                    let nameLit = LitT . StrTyLit $ tName ^. unqualifiedString
                    appT
                        (appT [t| (:>) |] [t| QueryParam $(pure nameLit) $(pure t0) |])
                        (params ts)
                t0 : ts -> do
                    tName <- case t0 of
                        ConT n -> pure n
                        VarT n -> pure n
                        err -> fail $ "Expected constructor parameter, got: " <> show err
                    let nameLit = LitT . StrTyLit $ tName ^. unqualifiedString
                    appT (appT [t| (:>) |] [t| Capture $(pure nameLit) $(pure t0) |])
                         (params ts)


prependServerEndpointName :: [UrlSegment] -> Type -> Q Type
prependServerEndpointName prefix rest = do
    foldr (\a b -> [t| $(pure a) :> $b |])
          (pure rest)
          (fmap (LitT . StrTyLit . view typed) prefix)

-- | Define a servant endpoint ending in a reference to the sub API.
-- `EditBook :: BookId -> BookCmd a -> Cmd a` will result in
-- "EditBook" :> Capture "BookId" BookId :> BookApi
epSubApiType :: SubApiData -> ReaderT [Name] Q Type
epSubApiType e = local (<> e ^. typed @ApiSpec . typed) $ do
    apiTypeName <- askApiTypeName

    bird        <- lift [t| (:>) |]
    let mkCapture :: Type -> Q Type
        mkCapture t = do
            let pName = LitT . StrTyLit $ getTypeName t
            [t| Capture $(pure pName) $(pure t) |]

        getTypeName :: Type -> String
        getTypeName = \case
            ConT n -> n ^. unqualifiedString
            _      -> "typename"
    captures <- lift $ traverse mkCapture $ e ^. field @"constructorArgs"
    lift $ prependServerEndpointName
        (e ^. typed)
        (foldr1 (\a b -> AppT (AppT bird a) b) (captures <> [ConT apiTypeName]))

mkEndpoints :: ApiSpec -> ReaderT [Name] Q [Dec]
mkEndpoints s = local (<> s ^. typed) $ traverse mkEpDec (s ^. typed)
  where
    mkEpDec :: ApiPiece -> ReaderT [Name] Q Dec
    mkEpDec e = do
        ty <- case s ^. typed @ServerType of
            CmdServer   -> cmdEndpointType e
            QueryServer -> queryEndpointType e
        epTypeName <- local (<> e ^. typed) askApiTypeName
        pure $ TySynD epTypeName [] ty

-- | Make command handlers for each endpoint
mkHandlers :: ApiSpec -> ReaderT [Name] Q [Dec]
mkHandlers spec = local (<> spec ^. typed) $ mconcat <$> traverse
    mkHandler
    (spec ^. typed @[ApiPiece])
  where
    mkHandler :: ApiPiece -> ReaderT [Name] Q [Dec]
    mkHandler ep = do
        runnerTy <- lift $ runnerType spec
        case (ep, serverType spec) of
            (Endpoint e, CmdServer  ) -> mkCmdEPHandler runnerTy e
            (Endpoint e, QueryServer) -> mkQueryEPHandler runnerTy e
            (SubApi   e, _          ) -> mkSubAPiHandler runnerTy e

mkQueryEPHandler :: Type -> EndpointData -> ReaderT [Name] Q [Dec]
mkQueryEPHandler runnerTy e = local (<> e ^. typed) $ do
    varNames <- traverse (const (lift $ newName "arg")) $ e ^. field @"constructorArgs"
    handlerRetType <- lift $ [t| Handler $(pure $ e ^. field @"handlerReturnType") |]
    handlerName <- askHandlerName
    let varPat = fmap VarP varNames
        nrArgs = length @[] $ e ^. field @"constructorArgs"
        funSig =
            SigD handlerName
                . AppT (AppT ArrowT runnerTy)
                $ case e ^. field @"constructorArgs" of
                      [] -> handlerRetType
                      ts -> foldr (AppT . AppT ArrowT) handlerRetType ts
        funBodyBase = AppE (VarE runner)
            $ foldl AppE (ConE $ e ^. field @"gadtName") (fmap VarE varNames)

        funBody = case e ^. field @"returnType" of
            TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
            _        -> pure $ funBodyBase
    funClause <- lift $ clause
        (pure (VarP runner) : (if nrArgs > 0 then (fmap pure varPat) else []))
        (normalB [| liftIO $ $(funBody)  |])
        []
    let funDef = FunD handlerName [funClause]
    pure [funSig, funDef]
  where
    runner :: Name
    runner = mkName "runner"

mkCmdEPHandler :: Type -> EndpointData -> ReaderT [Name] Q [Dec]
mkCmdEPHandler runnerTy e = local (<> e ^. typed) $ do
    varNames <- lift $ traverse (const $ newName "arg") $ e ^. field @"constructorArgs"
    handlerRetType <- lift [t| Handler $(pure $ e ^. field @"handlerReturnType") |]
    handlerName    <- askHandlerName
    bodyTag        <- askBodyTag
    let varPat = ConP nfName (fmap VarP varNames)
        nfName = mkName $ "NamedFields" <> show (length varNames)
        nrArgs = length @[] $ e ^. field @"constructorArgs"
        funSig =
            SigD handlerName
                . AppT (AppT ArrowT runnerTy)
                $ case e ^. field @"constructorArgs" of
                      [] -> handlerRetType
                      as ->
                          let nfType :: Type
                              nfType = AppT (ConT nfName) (LitT bodyTag)
                          in  AppT (AppT ArrowT (foldl AppT nfType as)) handlerRetType

        funBodyBase = AppE (VarE runner)
            $ foldl AppE (ConE $ e ^. field @"gadtName") (fmap VarE varNames)

        funBody = case e ^. field @"returnType" of
            TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
            _        -> pure $ funBodyBase
    funClause <- lift $ clause
        (pure (VarP runner) : (if nrArgs > 0 then [pure $ varPat] else []))
        (normalB [| liftIO $ $(funBody)  |])
        []
    let funDef = FunD handlerName [funClause]
    pure [funSig, funDef]
  where
    runner :: Name
    runner = mkName "runner"

mkSubAPiHandler :: Type -> SubApiData -> ReaderT [Name] Q [Dec]
mkSubAPiHandler runnerTy e = local (<> e ^. typed) $ do
    runner <- lift $ newName "runner"

    serverName <- askServerName
    apiTypeName <- askApiTypeName
    paramNames  <- traverse (const (lift $ newName "arg")) $ e ^. field @"constructorArgs"

    finalSig <- lift [t| Server $(pure $ ConT apiTypeName) |]
    params <- case e ^. field @"constructorArgs" of
        [] -> lift [t| $(pure runnerTy) -> $(pure finalSig) |]
        [a] -> lift [t| $(pure runnerTy) -> $(pure a) -> $(pure finalSig) |]
        ts -> pure $ foldr1 (\b a -> AppT (AppT ArrowT b) a)
                    (runnerTy : ts <> [finalSig])
    funSig <- SigD serverName <$> pure params

    funClause <- case fmap VarE paramNames of
        [] -> lift $ clause
                [varP runner]
                (fmap NormalB
                      [e| $(varE $ e ^. field @"subApi" . typed)
                             $ $(pure $ VarE runner)
                             . $(pure . ConE $ e ^. field @"gadtName") |]
                )
                []
        ts ->
            let cmd = foldl (\b a -> AppE b a) (ConE $ e ^. field @"gadtName") ts
             in lift $ clause
                  (varP <$>  runner : paramNames)
                  (fmap NormalB
                        [e| $(varE runner)
                                . $(pure cmd)
                        |]
                  )
                  []
    let funDef = FunD serverName [funClause]
    pure [funSig, funDef]

-- | The type of the CmdRunner used to execute commands
runnerType :: ApiSpec -> Q Type
runnerType spec = case serverType spec of
    QueryServer -> [t| QueryRunner $(pure . ConT $ spec ^. field @"gadtName") |]
    CmdServer   -> [t| CmdRunner $(pure . ConT $ spec ^. field @"gadtName") |]

-- | Create the full server and api dec
-- Assumes the handlers have already been created (using `mkHandlers`)
mkFullServer :: ApiSpec -> ReaderT [Name] Q [Dec]
mkFullServer spec = local (<> spec ^. typed) $ do
    runnerT <- lift $ runnerType spec
    let runner = mkName "runner"

        handlerExp :: Monad m => ApiPiece -> ReaderT [Name] m Exp
        handlerExp = \case
            Endpoint _ -> do
                hName <- askHandlerName
                pure $ VarE hName `AppE` VarE runner
            SubApi _ -> do
                serverName <- askServerName
                pure $ VarE serverName `AppE` VarE runner

    handlers <- traverse handlerExp $ endpoints spec
    body     <- case handlers of
        []     -> error "Server contains no endpoints"
        e : es -> lift $ foldM (\b a -> [| $(pure b) :<|> $(pure a) |]) e es

    -- We create the gadtName of the outer sever directly based on the GADT gadtName as this
    -- will be referenced directly from user code.
    let apiTypeName = spec ^. typed @Name & unqualifiedString <>~ "Api"
        serverName =
            spec
                ^. typed @Name
                &  (unqualifiedString <>~ "Server")
                &  (unqualifiedString %~ lowerFirst)
    apiPiecesTypeName <- traverse askApiPieceTypeName $ spec ^. typed

    apiDec            <- lift $ mkApiDec apiTypeName apiPiecesTypeName
    serverTypeDec     <- do
        ret <- lift [t| Server $(pure $ ConT apiTypeName) |]
        pure . SigD serverName $ AppT (AppT ArrowT runnerT) ret
    funDec <- do
        pure $ FunD serverName [Clause [VarP runner] (NormalB body) []]
    pure [apiDec, serverTypeDec, funDec]
