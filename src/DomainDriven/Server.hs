{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DefaultSignatures #-}

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
import           Data.Traversable
import           Data.Generics.Sum
import           GHC.Generics                   ( Generic )
import           DomainDriven.Internal.NamedFields
import           Control.Monad.Reader

data ApiSpec = ApiSpec
    { name       :: Name -- ^ Name of the GADT representing the command
    , urlPieces  :: [UrlSegment] -- ^ The Url path segments this API adds
    , endpoints  :: [ApiPiece] -- ^ Endpoints created from the constructors of the GADT
    , serverType :: ServerType -- ^ Type type of server represented
    }
    deriving (Show, Generic)

fullName :: Monad m => ReaderT [UrlSegment] m Name
fullName = do
    s <- ask
    pure . mkName $ mconcat (s ^.. folded . typed . to upperFirst)

askApiTypeName :: Monad m => ReaderT [UrlSegment] m Name
askApiTypeName = do
    n <- fullName
    pure $ n & unqualifiedString <>~ "Api" & unqualifiedString %~ upperFirst

askServerName :: Monad m => ReaderT [UrlSegment] m Name
askServerName = do
    s <- fullName
    pure $ s & unqualifiedString <>~ "Server" & unqualifiedString %~ lowerFirst

askEndpointTypeName :: Monad m => ReaderT [UrlSegment] m Name
askEndpointTypeName = do
    s <- fullName
    pure $ s & unqualifiedString <>~ "Endpoint" & unqualifiedString %~ upperFirst

askHandlerName :: Monad m => ReaderT [UrlSegment] m Name
askHandlerName = do
    s <- fullName
    pure $ s & unqualifiedString <>~ "Handler" & unqualifiedString %~ lowerFirst


askBodyTag :: Monad m => ReaderT [UrlSegment] m TyLit
askBodyTag = do
    s <- fullName
    pure . StrTyLit . show $ s & unqualifiedString <>~ "Body"


askApiPieceTypeName :: Monad m => ApiPiece -> ReaderT [UrlSegment] m Name
askApiPieceTypeName = \case
    Endpoint e -> localPath e $ askEndpointTypeName
    SubApi   e -> localPath e $ askApiTypeName


instance {-# Overlapping #-} HasType [UrlSegment] ApiPiece where
    typed = lens getter setter
      where
        getter = \case
            Endpoint e -> e ^. typed
            SubApi   e -> e ^. typed
        setter s a = case s of
            Endpoint e -> Endpoint $ e & typed .~ a
            SubApi   e -> SubApi $ e & typed .~ a

newtype UrlSegment = UrlSegment String
    deriving (Show, Generic, Eq)

mkUrlSegments :: ServerOptions -> Name -> [UrlSegment]
mkUrlSegments opts n =
    n ^.. unqualifiedString . to (renameConstructor opts) . folded . to UrlSegment

data ServerType
    = CmdServer -- ^ A server that runs commands, i.e. requires a command runner to run
    | QueryServer -- ^ A server that runs queries.
    deriving (Show, Generic)

data ApiPiece
    = Endpoint EndpointData
    | SubApi SubApiData
    deriving (Show, Generic)


data EndpointData = EndpointData
    { name              :: Name
    , urlPieces         :: [UrlSegment]
    , constructorArgs   :: [Type]
    , returnType        :: Type
    , handlerReturnType :: Type -- ^ Same as for constructor but with NoContent instead of ()
    }
    deriving (Show, Generic)

data SubApiData = SubApiData
    { name            :: Name
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
    GadtC [name] bangArgs (AppT _ retType) -> do
        hRetType <- case retType of
                                      TupleT 0 | opts ^. field @"unitIsNoContent" ->
                                          [t| NoContent|]
                                      t -> pure t

        pure . Endpoint $ EndpointData
            { name   = name
            , urlPieces    = mkUrlSegments opts name
            , constructorArgs   = fmap snd bangArgs
            , returnType        = retType
            , handlerReturnType = hRetType
            }
    -- When the constructor contain references to other domain models
    ForallC [KindedTV var StarT] [] (GadtC [name] bangArgs (AppT _ _retType)) -> do
        (args, subCmd') <- case reverse $ fmap snd bangArgs of
            AppT (ConT subCmd') (VarT var') : rest -> do
                unless (var == var')
                    $ error "Subcommand must be the last constructor argument"
                pure (reverse rest, subCmd')
            _ -> error $ "Last constructor argument must have form `SubCmd a`"

        subServerSpec <- mkServerSpec opts serverType subCmd'
        pure . SubApi $ SubApiData
            { name   = name
            , urlPieces    = mkUrlSegments opts name
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
    pure ApiSpec { name       = n
                 , urlPieces  = mkUrlSegments opts n
                 , endpoints  = eps
                 , serverType = serverType
                 }
  where
    -- FIXME: Improve error message to show the full name of offending types and
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

-- | Create type aliases for each endpoint, using constructor name prefixed with "Ep",
-- and a type `Api` that represents the full API.
mkCmdServer :: ServerOptions -> Name -> Q [Dec]
mkCmdServer opts name = do
    spec <- mkServerSpec opts CmdServer name
    runReaderT (mkServerFromSpec opts spec) []

mkQueryServer :: ServerOptions -> Name -> Q [Dec]
mkQueryServer opts name = do
    spec <- mkServerSpec opts QueryServer name
    runReaderT (mkServerFromSpec opts spec) []


data EndpointDec = EndpointDec
    { epTypeAlias :: Type
    , epTypeDec   :: Dec
    , epHandlerName :: Name
    , epHandlerDec :: Dec
    }
    deriving (Show, Generic)

mkServerFromSpec :: ServerOptions -> ApiSpec -> ReaderT [UrlSegment] Q [Dec]
mkServerFromSpec opts spec = localPath spec $ do
    let serverType :: ServerType
        serverType = spec ^. typed

    fullApiName <- askApiTypeName
    fullServerName <- askServerName

    epDecs <- undefined :: ReaderT [UrlSegment] Q [EndpointDec]

    case epDecs of
        e : es -> do
            apiTypeAlias <- fmap (TySynD fullApiName [])
                . lift $ foldM (\b a -> [| $(pure b) :<|> $(pure a) |])
                         (e ^. field @"epTypeAlias")
                         (fmap (^. field @"epTypeAlias") es)

        [] -> pure []
    let fullApiDec = TySynD fullApiName [] $
                e : es -> lift $ foldM (\b a -> [| $(pure b) :<|> $(pure a) |]) e es
        foldr (\a b -> [t| $(pure a) :<|> $(pure b)AppT  (VarT (e ^. field @"epTypeName")
    stuffs <- for (spec ^. typed @[ApiPiece]) $ \p -> localPath p $ do
        apiTypeName <- askApiTypeName p
        serverName  <- askServerName
        runnerType  <- lift $ mkRunnerType spec
        apiDec      <- mkApiDec spec

        let endpointDec :: [Dec]
            endpointDec = [TySynD tyName [] serverType]

        handlerDecs <- case (ep, serverType spec) of
            (Endpoint e, CmdServer  ) -> mkCmdEPHandler runnerType e
            (Endpoint e, QueryServer) -> mkQueryEPHandler runnerType e
            (SubApi   e, _          ) -> mkSubAPiHandler runnerType e
        serverTypeDec <- do
            ret <- lift [t| Server $(pure $ ConT apiTypeName) |]
            pure . SigD serverName $ AppT (AppT ArrowT runnerT) ret
        funDec <- do
            body <- case handlers of
                []     -> error "Server contains no endpoints"
                e : es -> lift $ foldM (\b a -> [| $(pure b) :<|> $(pure a) |]) e es
            pure $ FunD n [Clause [VarP runner] (NormalB body) []]
        pure ApiDecs { fullApiDeclaration        = apiDec
                        , endpointDeclaration   = endpointDec
                        , handlerDeclarations    = handlerDecs
                        , serverTypeDeclaration = serverTypeDec
                        , apiTypeDeclaration    = apiTypeDec
                        , serverDeclaration   = funDec
                        }

    let mkSubServer :: ApiSpec -> ReaderT [UrlSegment] Q [Dec]
        mkSubServer = localPath s . mkServerFromSpec opts
    subServers <-
        fmap mconcat
        .   traverse mkSubServer
        $   s
        ^.. typed @[ApiPiece]
        .   folded
        .   _Typed @SubApiData
        .   typed
    endpointDecs <- fmap mconcat . traverse (mkEndpointDec (s ^. typed)) $ s ^. typed
    handlers     <- mkHandlers s
    server       <- mkFullServer s
    pure $ subServers <> endpointDecs <> handlers <> server

mkRunnerType :: ApiSpec -> Q Type
mkRunnerType spec = case serverType spec of
    QueryServer -> [t| QueryRunner $(pure . ConT $ spec ^. field @"name") |]
    CmdServer   -> [t| CmdRunner $(pure . ConT $ spec ^. field @"name") |]

mkHandlers :: ApiSpec -> ReaderT [UrlSegment] Q [Dec]
mkHandlers spec = fmap mconcat . forM (endpoints spec) $ \ep -> do
    runnerTy <- lift $ mkRunnerType spec
    case (ep, serverType spec) of
        (Endpoint e, CmdServer  ) -> localPath e $ mkCmdEPHandler runnerTy e
        (Endpoint e, QueryServer) -> localPath e $ mkQueryEPHandler runnerTy e
        (SubApi   e, _          ) -> localPath e $ mkSubAPiHandler runnerTy e


-- | Create a typealias for the API
-- type Api = EpA :<|> EpB :<|> EpC ...
mkApiDec :: ApiSpec -> ReaderT [UrlSegment] Q Dec
mkApiDec spec = do
    apiName       <- askApiTypeName
    apiPieceNames <- traverse askApiPieceTypeName $ spec ^. typed @[ApiPiece]
    TySynD apiName [] <$> case apiPieceNames of
        []     -> error "Server has no endpoints"
        x : xs -> do
            let f :: Type -> Name -> ReaderT a Q Type
                f b a = lift [t| $(pure b) :<|> $(pure $ ConT a) |]
            foldM f (ConT x) xs

-- | Define a type alias representing the type of the endpoint
mkEndpointDec :: ServerType -> ApiPiece -> ReaderT [UrlSegment] Q [Dec]
mkEndpointDec sType e = localPath e $ do
    ty <- case sType of
        CmdServer   -> cmdEndpointType e
        QueryServer -> queryEndpointType e
    apiPieceTypeName <- askApiPieceTypeName e
    pure $ [TySynD apiPieceTypeName [] ty]


-- | Create the full server and api dec
-- Assumes the handlers have already been created (using `mkHandlers`)
mkFullServer' :: ApiSpec -> ReaderT [UrlSegment] Q [Dec]
mkFullServer' spec = do
    runnerT <- lift $ mkRunnerType spec
    let runner = mkName "runner"

        handlerExp :: Monad m => ApiPiece -> ReaderT [UrlSegment] m Exp
        handlerExp = \case
            Endpoint e -> localPath e $ do
                hName <- askHandlerName
                pure $ VarE hName `AppE` VarE runner
            SubApi e -> localPath e $ do
                serverName <- askServerName
                pure $ VarE serverName `AppE` VarE runner

    handlers <- traverse handlerExp $ endpoints spec
    body     <- case handlers of
        []     -> error "Server contains no endpoints"
        e : es -> lift $ foldM (\b a -> [| $(pure b) :<|> $(pure a) |]) e es
    -- apiDec        <- mkApiDec spec
    --serverTypeDec <- do
    --    serverName  <- askServerName
    --    apiTypeName <- askApiTypeName
    --    ret         <- lift [t| Server $(pure $ ConT apiTypeName) |]
    --    pure . SigD serverName $ AppT (AppT ArrowT runnerT) ret
    funDec <- do
        n <- askServerName
        pure $ FunD n [Clause [VarP runner] (NormalB body) []]
    pure [apiDec, serverTypeDec, funDec]

-- | Create the full server and api dec
-- Assumes the handlers have already been created (using `mkHandlers`)
mkFullServer :: ApiSpec -> ReaderT [UrlSegment] Q [Dec]
mkFullServer spec = do
    runnerT <- lift $ mkRunnerType spec
    let runner = mkName "runner"

        handlerExp :: Monad m => ApiPiece -> ReaderT [UrlSegment] m Exp
        handlerExp = \case
            Endpoint e -> localPath e $ do
                hName <- askHandlerName
                pure $ VarE hName `AppE` VarE runner
            SubApi e -> localPath e $ do
                serverName <- askServerName
                pure $ VarE serverName `AppE` VarE runner

    handlers <- traverse handlerExp $ endpoints spec
    body     <- case handlers of
        []     -> error "Server contains no endpoints"
        e : es -> lift $ foldM (\b a -> [| $(pure b) :<|> $(pure a) |]) e es
    apiDec        <- mkApiDec spec
    serverTypeDec <- do
        serverName  <- askServerName
        apiTypeName <- askApiTypeName
        ret         <- lift [t| Server $(pure $ ConT apiTypeName) |]
        pure . SigD serverName $ AppT (AppT ArrowT runnerT) ret
    funDec <- do
        n <- askServerName
        pure $ FunD n [Clause [VarP runner] (NormalB body) []]
    pure [apiDec, serverTypeDec, funDec]
-- | Create a request body by turning multiple arguments into a NamedFieldsN
-- `BuyThing ItemKey Quantity`
-- yields:
-- `ReqBody '[JSON] (NamedFields2 "BuyThing" ItemKey Quantity)`
mkReqBody :: EndpointData -> ReaderT [UrlSegment] Q (Maybe Type)
mkReqBody e = do
    unless
        (args == L.nub args)
        (fail
        $ "Each argument to the constructor must have a unique type.\
           \\nGot request body containing: "
        <> L.intercalate ", " (args ^.. folded . types @Name . to show)
        )
    body <- mkBody
    case body of
        Nothing -> pure Nothing
        Just b  -> Just <$> lift [t| ReqBody '[JSON] $(pure b) |]
  where
    args :: [Type]
    args = e ^. field @"constructorArgs"


    mkBody :: ReaderT [UrlSegment] Q (Maybe Type)
    mkBody = case args of
        [] -> pure Nothing
        ts -> do
            bodyTag <- askBodyTag
            let n = length ts
                constructor =
                    AppT (ConT (mkName $ "NamedFields" <> show n)) (LitT bodyTag)
            pure . Just $ foldl AppT constructor ts

-- | Tag used as the symbol in in `NamedFieldsN`.
-- This field is used to name the type in the OpenAPI definition.
--mkBodyTag :: EndpointData -> String
--mkBodyTag e = mconcat . (<> ["Body"]) $ prefixes & traversed . _head %~ toUpper
--  where
--    prefixes :: [String]
--    prefixes = e ^.. field @"urlPieces" . folded . to show

-- | Define the servant endpoint type for non-hierarchical command constructors. E.g.
-- `BuyBook :: BookId -> Integer -> Cmd ()` will result in:
-- "BuyBook" :> ReqBody '[JSON] (BookId, Integer) -> Post '[JSON] NoContent
cmdEndpointType :: ApiPiece -> ReaderT [UrlSegment] Q Type
cmdEndpointType = \case
    SubApi   e -> epSubApiType e
    Endpoint e -> do
        middle <- reqBody >>= \case
            Nothing -> lift reqReturn
            Just b  -> lift [t| $(pure b) :>  $reqReturn |]
        mkServantEpName middle
      where
        reqBody :: ReaderT [UrlSegment] Q (Maybe Type)
        reqBody = mkReqBody e

        reqReturn :: Q Type
        reqReturn = [t| Post '[JSON] $(pure $ e ^. field @"handlerReturnType") |]

-- | Define the servant endpoint type for non-hierarchical query constructors. E.g.
-- `GetBook :: BookId -> Query Book` will result in:
-- "GetBook" :> Capture "BookId" BookId" :> Get '[JSON] Book
queryEndpointType :: ApiPiece -> ReaderT [UrlSegment] Q Type
queryEndpointType = \case
    SubApi   e -> epSubApiType e
    Endpoint e -> mkServantEpName =<< lift (params (e ^. field @"constructorArgs"))
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


mkServantEpName :: Type -> ReaderT [UrlSegment] Q Type
mkServantEpName rest = do
    prefix <- ask
    lift $ foldr (\a b -> [t| $(pure a) :> $b |])
                 (pure rest)
                 (fmap (LitT . StrTyLit . view typed) prefix)

-- | Define a servant endpoint ending in a reference to the sub API.
-- `EditBook :: BookId -> BookCmd a -> Cmd a` will result in
-- "EditBook" :> Capture "BookId" BookId :> BookApi
epSubApiType :: SubApiData -> ReaderT [UrlSegment] Q Type
epSubApiType e = localPath e $ do

    let subApiType :: Type
        subApiType = ConT $ e ^. typed @ApiSpec . typed @Name

    bird <- lift [t| (:>) |]
    let mkCapture :: Type -> Q Type
        mkCapture t = do
            let pName = LitT . StrTyLit $ getTypeName t
            [t| Capture $(pure pName) $(pure t) |]

        getTypeName :: Type -> String
        getTypeName = \case
            ConT n -> n ^. unqualifiedString
            _      -> "typename"
    captures <- lift $ traverse mkCapture $ e ^. field @"constructorArgs"
    mkServantEpName (foldr1 (\a b -> AppT (AppT bird a) b) (captures <> [subApiType]))


localPath
    :: (Monad m, HasType [UrlSegment] a)
    => a
    -> ReaderT [UrlSegment] m b
    -> ReaderT [UrlSegment] m b
localPath a = local (<> a ^. typed)

-- | Make command handlers for each endpoint
mkQueryEPHandler :: Type -> EndpointData -> ReaderT [UrlSegment] Q [Dec]
mkQueryEPHandler runnerTy e = do
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
            $ foldl AppE (ConE $ e ^. field @"name") (fmap VarE varNames)

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

mkCmdEPHandler :: Type -> EndpointData -> ReaderT [UrlSegment] Q [Dec]
mkCmdEPHandler runnerTy e = do
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
            $ foldl AppE (ConE $ e ^. field @"name") (fmap VarE varNames)

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

mkSubAPiHandler :: Type -> SubApiData -> ReaderT [UrlSegment] Q [Dec]
mkSubAPiHandler runnerTy e = do
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
                             . $(pure . ConE $ e ^. field @"name") |]
                )
                []
        ts ->
            let cmd = foldl (\b a -> AppE b a) (ConE $ e ^. field @"name") ts
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
