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

data ApiSpec = ApiSpec
    { name       :: Name -- ^ Name of the GADT representing the command
    , urlPieces  :: [UrlSegment] -- ^ The Url path segments this API adds
    , endpoints  :: [ApiPiece] -- ^ Endpoints created from the constructors of the GADT
    , serverType :: ServerType
    }
    deriving (Show, Generic)

fullName :: [UrlSegment] -> Name
fullName s = mkName $ mconcat (s ^.. folded . typed . to upperFirst)

apiTypeName :: [UrlSegment] -> Name
apiTypeName s =
    fullName s & unqualifiedString <>~ "Api" & unqualifiedString %~ upperFirst

serverName :: [UrlSegment] -> Name
serverName s =
    fullName s & unqualifiedString <>~ "Server" & unqualifiedString %~ lowerFirst

endpointTypeName :: [UrlSegment] -> Name
endpointTypeName s =
    fullName s & unqualifiedString <>~ "Endpoint" & unqualifiedString %~ upperFirst

handlerName :: [UrlSegment] -> Name
handlerName s =
    fullName s & unqualifiedString <>~ "Handler" & unqualifiedString %~ lowerFirst


bodyTag :: [UrlSegment] -> TyLit
bodyTag s = StrTyLit . show $ fullName s & unqualifiedString <>~ "Body"


apiPieceTypeName :: [UrlSegment] -> ApiPiece -> Name
apiPieceTypeName prefix = \case
    Endpoint e -> endpointTypeName $ prefix <> e ^. typed
    SubApi   e -> apiTypeName $ prefix <> e ^. typed

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
mkCmdServer opts = mkServerFromSpec opts [] <=< mkServerSpec opts CmdServer

mkQueryServer :: ServerOptions -> Name -> Q [Dec]
mkQueryServer opts = mkServerFromSpec opts [] <=< mkServerSpec opts QueryServer

mkServerFromSpec :: ServerOptions -> [UrlSegment] -> ApiSpec -> Q [Dec]
mkServerFromSpec opts prefix s = do
    let mkSubServer :: ApiSpec -> Q [Dec]
        mkSubServer = mkServerFromSpec opts (prefix <> s ^. typed)
    subServers <-
        fmap mconcat
        .   traverse mkSubServer
        $   s
        ^.. typed @[ApiPiece]
        .   folded
        .   _Typed @SubApiData
        .   typed :: Q [Dec]
    endpointDecs <-
        fmap mconcat . traverse (mkEndpointDec (s ^. typed) prefix) $ s ^. typed :: Q
            [Dec]
    handlers <- mkHandlers prefix s :: Q [Dec]
    server   <- mkFullServer prefix s :: Q [Dec]
    pure $ subServers <> endpointDecs <> handlers <> server

-- | Create a typealias for the API
-- type Api = EpA :<|> EpB :<|> EpC ...
mkApiDec :: [UrlSegment] -> ApiSpec -> Q Dec
mkApiDec prefix spec =
    TySynD (apiTypeName $ prefix <> spec ^. typed) []
        <$> case apiPieceTypeName prefix <$> spec ^.. typed @[ApiPiece] . folded of
                []     -> error "Server has no endpoints"
                x : xs -> do
                    let f :: Type -> Name -> Q Type
                        f b a = [t| $(pure b) :<|> $(pure $ ConT a) |]
                    foldM f (ConT x) xs

-- | Create a request body by turning multiple arguments into a NamedFieldsN
-- `BuyThing ItemKey Quantity`
-- yields:
-- `ReqBody '[JSON] (NamedFields2 "BuyThing" ItemKey Quantity)`
mkReqBody :: EndpointData -> Q (Maybe Type)
mkReqBody e = do
    unless
        (args == L.nub args)
        (fail
        $ "Each argument to the constructor must have a unique type.\
           \\nGot request body containing: "
        <> L.intercalate ", " (args ^.. folded . types @Name . to show)
        )
    mkBody >>= maybe (pure Nothing) (\b -> fmap Just [t| ReqBody '[JSON] $(pure b) |])
  where
    args :: [Type]
    args = e ^. field @"constructorArgs"


    mkBody :: Q (Maybe Type)
    mkBody = case args of
        [] -> pure Nothing
        ts -> do
            let n           = length ts
                constructor = AppT (ConT (mkName $ "NamedFields" <> show n))
                                   (LitT $ e ^. typed . to bodyTag)
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
cmdEndpointType :: [UrlSegment] -> ApiPiece -> Q Type
cmdEndpointType prefix = \case
    Endpoint e -> epSimpleType e
    SubApi   e -> epSubApiType prefix e
  where
    epSimpleType :: EndpointData -> Q Type
    epSimpleType e =
        [t| $(mkServantEpName (e ^. field @"urlPieces") middle) |]
      where
        middle :: Q Type
        middle = reqBody >>= \case
            Nothing -> reqReturn
            Just b  -> [t| $(pure b) :>  $reqReturn |]

        reqBody :: Q (Maybe Type)
        reqBody = mkReqBody e

        reqReturn :: Q Type
        reqReturn = [t| Post '[JSON] $(pure $ e ^. field @"handlerReturnType") |]

-- | Define the servant endpoint type for non-hierarchical query constructors. E.g.
-- `GetBook :: BookId -> Query Book` will result in:
-- "GetBook" :> Capture "BookId" BookId" :> Get '[JSON] Book
queryEndpointType :: [UrlSegment] -> ApiPiece -> Q Type
queryEndpointType prefix = \case
    Endpoint e -> epSimpleType e
    SubApi   e -> epSubApiType prefix e
  where
    epSimpleType :: EndpointData -> Q Type
    epSimpleType e =
        [t| $(mkServantEpName (e ^. field @"urlPieces") (params $ e ^. field @"constructorArgs")) |]
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


mkServantEpName :: [UrlSegment] -> Q Type -> Q Type
mkServantEpName prefix rest = foldr (\a b -> [t| $(pure a) :> $b |])
                                    rest
                                    (fmap (LitT . StrTyLit . view typed) prefix)

-- | Define a servant endpoint ending in a reference to the sub API.
-- `EditBook :: BookId -> BookCmd a -> Cmd a` will result in
-- "EditBook" :> Capture "BookId" BookId :> BookApi
epSubApiType :: [UrlSegment] -> SubApiData -> Q Type
epSubApiType prefix e = do

    let subApiType :: Type
        subApiType = ConT $ e ^. typed @ApiSpec . typed @Name

    bird <- [t| (:>) |]
    let mkCapture :: Type -> Q Type
        mkCapture t = do
            let pName = LitT . StrTyLit $ getTypeName t
            [t| Capture $(pure pName) $(pure t) |]

        getTypeName :: Type -> String
        getTypeName = \case
            ConT n -> n ^. unqualifiedString
            _      -> "typename"
    captures <- traverse mkCapture $ e ^. field @"constructorArgs"
    mkServantEpName
        (prefix <> e ^. field @"urlPieces")
        (pure $ foldr1 (\a b -> AppT (AppT bird a) b) (captures <> [subApiType]))

-- | Define a type alias representing the type of the endpoint
mkEndpointDec :: ServerType -> [UrlSegment] -> ApiPiece -> Q [Dec]
mkEndpointDec sType prefix e = do
    ty <- case sType of
        CmdServer   -> cmdEndpointType prefix e
        QueryServer -> queryEndpointType prefix e
    pure $ [TySynD (apiPieceTypeName prefix e) [] ty]


-- | Make command handlers for each endpoint
mkHandlers :: [UrlSegment] -> ApiSpec -> Q [Dec]
mkHandlers prefix spec = fmap mconcat . forM (endpoints spec) $ \ep -> do
    runnerTy <- runnerType spec
    case (ep, serverType spec) of
        (Endpoint e, CmdServer  ) -> mkCmdEPHandler runnerTy prefix e
        (Endpoint e, QueryServer) -> mkQueryEPHandler runnerTy prefix e
        (SubApi   e, _          ) -> mkSubAPiHandler runnerTy prefix e

mkQueryEPHandler :: Type -> [UrlSegment] -> EndpointData -> Q [Dec]
mkQueryEPHandler runnerTy prefix e = do
    varNames       <- traverse (const $ newName "arg") $ e ^. field @"constructorArgs"
    handlerRetType <- [t| Handler $(pure $ e ^. field @"handlerReturnType") |]
    let varPat = fmap VarP varNames
        nrArgs = length @[] $ e ^. field @"constructorArgs"
        funSig =
            SigD (handlerName $ prefix <> e ^. typed)
                . AppT (AppT ArrowT runnerTy)
                $ case e ^. field @"constructorArgs" of
                      [] -> handlerRetType
                      ts -> foldr (AppT . AppT ArrowT) handlerRetType ts
        funBodyBase = AppE (VarE runner)
            $ foldl AppE (ConE $ e ^. field @"name") (fmap VarE varNames)

        funBody = case e ^. field @"returnType" of
            TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
            _        -> pure $ funBodyBase
    funClause <- clause
        (pure (VarP runner) : (if nrArgs > 0 then (fmap pure varPat) else []))
        (normalB [| liftIO $ $(funBody)  |])
        []
    let funDef = FunD (handlerName $ prefix <> e ^. typed) [funClause]
    pure [funSig, funDef]
  where
    runner :: Name
    runner = mkName "runner"

mkCmdEPHandler :: Type -> [UrlSegment] -> EndpointData -> Q [Dec]
mkCmdEPHandler runnerTy prefix e = do
    varNames       <- traverse (const $ newName "arg") $ e ^. field @"constructorArgs"
    handlerRetType <- [t| Handler $(pure $ e ^. field @"handlerReturnType") |]
    let varPat = ConP name (fmap VarP varNames)
        name   = mkName $ "NamedFields" <> show (length varNames)
        nrArgs = length @[] $ e ^. field @"constructorArgs"
        funSig =
            SigD (handlerName $ prefix <> e ^. typed)
                . AppT (AppT ArrowT runnerTy)
                $ case e ^. field @"constructorArgs" of
                      [] -> handlerRetType
                      as ->
                          let nfType :: Type
                              nfType = AppT (ConT name) (LitT $ e ^. typed . to bodyTag)
                          in  AppT (AppT ArrowT (foldl AppT nfType as)) handlerRetType

        funBodyBase = AppE (VarE runner)
            $ foldl AppE (ConE $ e ^. field @"name") (fmap VarE varNames)

        funBody = case e ^. field @"returnType" of
            TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
            _        -> pure $ funBodyBase
    funClause <- clause
        (pure (VarP runner) : (if nrArgs > 0 then [pure $ varPat] else []))
        (normalB [| liftIO $ $(funBody)  |])
        []
    let funDef = FunD (handlerName $ prefix <> e ^. typed) [funClause]
    pure [funSig, funDef]
  where
    runner :: Name
    runner = mkName "runner"

mkSubAPiHandler :: Type -> [UrlSegment] -> SubApiData -> Q [Dec]
mkSubAPiHandler runnerTy prefix e = do
    runner <- newName "runner"

    paramNames  <- traverse (const $ newName "arg") $ e ^. field @"constructorArgs"
    let finalSig = [t| Server $(pure . ConT $ apiTypeName $ prefix <> e ^. field @"subApi" . typed) |]
    finalSig' <- finalSig
    params <- case e ^. field @"constructorArgs" of
        [] -> [t| $(pure runnerTy) -> $finalSig |]
        [a] -> [t| $(pure runnerTy) -> $(pure a) -> $finalSig |]
        ts -> pure $ foldr1 (\b a -> AppT (AppT ArrowT b) a)
                    (runnerTy : ts <> [finalSig'])
    funSig <- SigD (serverName $ prefix <> e ^. typed) <$> pure params

    funClause <- case fmap VarE paramNames of
        [] -> clause
                [varP runner]
                (fmap NormalB
                      [e| $(varE $ e ^. field @"subApi" . typed)
                             $ $(pure $ VarE runner)
                             . $(pure . ConE $ e ^. field @"name") |]
                )
                []
        ts ->
            let cmd = foldl (\b a -> AppE b a) (ConE $ e ^. field @"name") ts
             in clause
                  (varP <$>  runner : paramNames)
                  (fmap NormalB
                        [e| $(varE runner)
                                . $(pure cmd)
                        |]
                  )
                  []
    let funDef = FunD (serverName $ prefix <> e ^. typed) [funClause]
    pure [funSig, funDef]

-- | The type of the CmdRunner used to execute commands
runnerType :: ApiSpec -> Q Type
runnerType spec = case serverType spec of
    QueryServer -> [t| QueryRunner $(pure . ConT $ spec ^. field @"name") |]
    CmdServer   -> [t| CmdRunner $(pure . ConT $ spec ^. field @"name") |]

-- | Create the full server and api dec
-- Assumes the handlers have already been created (using `mkHandlers`)
mkFullServer :: [UrlSegment] -> ApiSpec -> Q [Dec]
mkFullServer prefix spec = do
    runnerT <- runnerType spec
    let runner = mkName "runner"

        handlerExp :: ApiPiece -> Exp
        handlerExp = \case
            Endpoint e -> VarE (handlerName $ prefix <> e ^. typed) `AppE` VarE runner
            SubApi   e -> VarE (serverName $ prefix <> e ^. typed) `AppE` VarE runner

    body <- case handlerExp <$> endpoints spec of
        []     -> error "Server contains no endpoints"
        e : es -> foldM (\b a -> [| $(pure b) :<|> $(pure a) |]) e es
    apiDec        <- mkApiDec prefix spec
    serverTypeDec <-
        SigD (serverName $ prefix <> spec ^. typed)
        .   AppT (AppT ArrowT runnerT)
        <$> [t| Server $(pure $ ConT $ apiTypeName $ prefix <> spec ^. typed ) |]
    let funDec = FunD (serverName $ prefix <> spec ^. typed)
                      [Clause [VarP runner] (NormalB body) []]
    pure [apiDec, serverTypeDec, funDec]
