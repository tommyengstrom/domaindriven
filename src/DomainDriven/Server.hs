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
import           Data.Traversable
import           Data.Char
import           Control.Monad.Trans
import           Control.Lens
import           Data.Generics.Product
import           GHC.Generics                   ( Generic )
import           DomainDriven.Internal.NamedFields
import           Control.Monad.Reader

data ApiSpec = ApiSpec
    { gadtName   :: GadtName -- ^ Name of the GADT representing the command
    , endpoints  :: [ApiPiece] -- ^ Endpoints created from the constructors of the GADT
    , apiOptions :: ApiOptions -- ^ The setting to use when generating part of the API
    }
    deriving (Show, Generic)

data ApiPiece
    = Endpoint ConstructorName ConstructorArgs Type
    | SubApi ConstructorName ConstructorArgs ApiSpec
    deriving (Show, Generic)

data ApiOptions = ApiOptions
    { renameConstructor :: String -> [String]
    , typenameSeparator :: String
    }
    deriving Generic

instance Show ApiOptions  where
    show o =
        "ApiOptions {renameConstructor = ***, typenameSeparator = \""
            <> o
            ^. field @"typenameSeparator"
            <> "\"}"

newtype ConstructorName = ConstructorName Name
    deriving (Show, Generic, Eq)

newtype GadtName = GadtName Name
    deriving (Show, Generic, Eq)

newtype UrlSegment = UrlSegment String
    deriving (Show, Generic, Eq)

newtype ConstructorArgs = ConstructorArgs [Type]
    deriving (Show, Generic, Eq)

newtype Runner = Runner Type
    deriving (Show, Generic, Eq)

--    pure . StrTyLit . show $ s & unqualifiedString <>~ "Body"
--
--
--askApiPieceTypeName :: Monad m => ApiPiece -> ReaderT [Name] m Name
--askApiPieceTypeName = \case
--    Endpoint e -> local (<> e ^. typed) askApiTypeName
--    SubApi   e -> local (<> e ^. typed) askApiTypeName
--
--mkUrlSegments :: ApiOptions -> ConstructorName -> [UrlSegment]
--mkUrlSegments opts n =
--    n ^.. typed . unqualifiedString . to (renameConstructor opts) . folded . to UrlSegment

mkUrlSegments :: Monad m => ConstructorName -> ReaderT ServerInfo m [UrlSegment]
mkUrlSegments n = do
    opts <- asks (view typed)
    pure
        $   n
        ^.. typed
        .   unqualifiedString
        .   to (renameConstructor opts)
        .   folded
        .   to UrlSegment



defaultApiOptions :: ApiOptions
defaultApiOptions = ApiOptions { renameConstructor = pure, typenameSeparator = "_" }

getCmdDec :: GadtName -> Q Dec
getCmdDec (GadtName n) = do
    cmdType <- reify n
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

mkApiPiece :: ApiOptions -> Con -> Q ApiPiece
mkApiPiece opts = \case
    -- The normal case
    GadtC [gadtName] bangArgs (AppT _ retType) -> do

        pure $ Endpoint (ConstructorName gadtName)
                        (ConstructorArgs $ fmap snd bangArgs)
                        retType
    -- When the constructor contain references to other domain models
    ForallC [KindedTV var StarT] [] (GadtC [gadtName] bangArgs (AppT _ _retType)) -> do
        (args, subCmd') <- case reverse $ fmap snd bangArgs of
            AppT (ConT subCmd') (VarT var') : rest -> do
                unless (var == var')
                    $ error "Subcommand must be the last constructor argument"
                pure (reverse rest, subCmd')
            _ -> error $ "Last constructor argument must have form `SubCmd a`"

        subServerSpec <- mkServerSpec opts (GadtName subCmd')
        pure $ SubApi (ConstructorName gadtName) (ConstructorArgs args) subServerSpec
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
mkServerSpec :: ApiOptions -> GadtName -> Q ApiSpec
mkServerSpec opts n = do
    eps <- traverse (mkApiPiece opts) =<< getConstructors =<< getCmdDec n

    pure ApiSpec { gadtName = n, endpoints = eps, apiOptions = opts }

-- | Verifies that the server do not generate overlapping paths
verifySpec :: ApiSpec -> Q ()
verifySpec _ = pure ()
    -- where
    --   -- FIXME: Improve error message to show the full gadtName of offending types and
    --   -- constructors
    --   ensureUniquePaths :: [Endpoint] -> Q ()
    --   ensureUniquePaths eps = do
    --       let paths :: [(Name, String)]
    --           paths      = L.sortOn snd $ foldMap (fullConstructorPaths opts) eps

    --           duplicates = L.nub $ mconcat $ zipWith
    --               (\a b -> if snd a == snd b then [a, b] else [])
    --               paths
    --               (drop 1 paths)

    --       unless
    --           (null duplicates)
    --           (fail $ "Api contains duplicated paths:\n * " <> L.intercalate
    --               "\n * "
    --               (show <$> duplicates)
    --           )

    -- fullConstructorPaths :: ApiOptions -> ApiPiece -> [(Name, String)]
    -- fullConstructorPaths opts = \case
    --     Endpoint a ->
    --         [ ( a ^. typed
    --           , L.intercalate "/" $ a ^. typed @[UrlSegment] & traversed %~ view typed
    --           )
    --         ]
    --     SubApi a -> mconcat $ a ^.. typed @ApiSpec . typed @[Endpoint] . folded . to
    --         (fullConstructorPaths opts)
    --
-- | Create type aliases for each endpoint, using constructor gadtName prefixed with "Ep",
-- and a type `Api` that represents the full API.


------------------------------------------------------------------------------------------

-- |  Generate the server from the spec
mkCmdServer :: ApiOptions -> Name -> Q [Dec]
mkCmdServer opts gadtName = do
    spec <- mkServerSpec opts (GadtName gadtName)
    verifySpec spec
    let si :: ServerInfo
        si = ServerInfo { baseGadt           = spec ^. typed
                        , parentConstructors = []
                        , prefixSegments     = []
                        , options            = opts
                        }
    runReaderT (mkServerFromSpec spec) si

-- | Carries information regarding how the API looks at the place we're currently at.
data ServerInfo = ServerInfo
    { baseGadt           :: GadtName -- ^ Use as a prefix of all types
    , parentConstructors :: [ConstructorName] -- ^ To create good names without conflict
    , prefixSegments     :: [UrlSegment] -- ^ Used to give a good name to the request body
    , options            :: ApiOptions -- ^ The current options
    }
    deriving (Show, Generic)

-- qualifiedName :: Monad m => Name -> ReaderT [Name] m Name
-- qualifiedName n = do
--     s <- ask
--     pure . mkName . mconcat . fmap show $ s <> [n]
--
askTypeName :: Monad m => ReaderT ServerInfo m Name
askTypeName = do
    si <- ask
    let baseName :: String
        baseName = si ^. typed @GadtName . typed @Name . unqualifiedString

        cNames :: [String]
        cNames =
            si ^.. typed @[ConstructorName] . folded . typed @Name . unqualifiedString
        separator :: String
        separator = si ^. typed @ApiOptions . field @"typenameSeparator"

    pure . mkName . L.intercalate separator $ baseName : cNames


askApiTypeName :: Monad m => ReaderT ServerInfo m Name
askApiTypeName = (unqualifiedString <>~ "Api") <$> askTypeName

askEndpointTypeName :: Monad m => ReaderT ServerInfo m Name
askEndpointTypeName = (unqualifiedString <>~ "Endpoint") <$> askTypeName

askServerName :: Monad m => ReaderT ServerInfo m Name
askServerName =
    (\n -> n & unqualifiedString %~ lowerFirst & unqualifiedString <>~ "Server")
        <$> askTypeName

askHandlerName :: Monad m => ReaderT ServerInfo m Name
askHandlerName =
    (\n -> n & unqualifiedString %~ lowerFirst & unqualifiedString <>~ "Handler")
        <$> askTypeName
-- askHandlerName :: Monad m => Name -> ReaderT [Name] m Name
-- askHandlerName n =
--     (unqualifiedString <>~ "Handler")
--         .   (unqualifiedString %~ lowerFirst)
--         <$> qualifiedName n
--
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- I need to create a structure that contians both Name and [UrlSegment] to use for the
-- reader. BodyTag should have the "exposed name" but the rest of the stuff should have
-- the normal names without any mapping.
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
askBodyTag :: Monad m => ConstructorName -> ReaderT ServerInfo m TyLit
askBodyTag n = do
    urlSegments <- asks (view $ typed @[UrlSegment])
    separator   <- asks (view $ typed @ApiOptions . field @"typenameSeparator")
    pure . StrTyLit . L.intercalate separator $ urlSegments ^.. folded . typed

enterApi :: Monad m => ApiSpec -> ReaderT ServerInfo m a -> ReaderT ServerInfo m a
enterApi s = local extendServerInfo
  where
    extendServerInfo :: ServerInfo -> ServerInfo
    extendServerInfo i = i & typed .~ s ^. typed @ApiOptions

enterApiPiece :: Monad m => ApiPiece -> ReaderT ServerInfo m a -> ReaderT ServerInfo m a
enterApiPiece p m = do
    newSegments <- mkUrlSegments (p ^. typed)
    let extendServerInfo :: ServerInfo -> ServerInfo
        extendServerInfo i =
            i
                & (typed @[UrlSegment] <>~ newSegments)
                & (typed @[ConstructorName] <>~ p ^. typed . to pure)

    local extendServerInfo m

--        case (ep, serverType spec) of
--            (Endpoint e, CmdServer  ) -> mkCmdEPHandler runnerTy e
--            (SubApi   e, _          ) -> mkSubAPiHandler runnerTy e

---- | This is the only layer of the ReaderT stack where we do not use `local` to update the
---- url segments.
mkServerFromSpec :: ApiSpec -> ReaderT ServerInfo Q [Dec]
mkServerFromSpec spec = enterApi spec $ do
    apiTypeName <- askApiTypeName
    serverName  <- askServerName
    apiTypeDec  <- do
        -- let askApiPieceTypeName :: ApiPiece -> ReaderT ServerInfo Q Name
        --     askApiPieceTypeName p = enterApiPiece p $ case p of
        --         Endpoint{} -> askEndpointTypeName
        --         SubApi{}   -> askApiTypeName

        -- epTypeAliasNames <- traverse askApiPieceTypeName (spec ^. typed @[ApiPiece])
        let mkApiPieceType :: ApiPiece -> ReaderT ServerInfo Q Type
            mkApiPieceType p = enterApiPiece p $ case p of
                Endpoint{}       -> ConT <$> askEndpointTypeName
                SubApi cName _ _ -> do
                    urlSegments <- mkUrlSegments cName
                    n           <- askApiTypeName
                    lift $ prependServerEndpointName urlSegments (ConT n)
        epTypes <- traverse mkApiPieceType (spec ^. typed @[ApiPiece])
        case epTypes of
            []     -> error "Server contains no endpoints"
            t : ts -> do
                let fish :: Type -> Type -> Q Type
                    fish b a = [t| $(pure b) :<|> $(pure a) |]
                TySynD apiTypeName [] <$> lift (foldM fish t ts)
    epTypeDecs <- fmap mconcat $ for (spec ^. typed @[ApiPiece]) $ \p ->
        enterApiPiece p $ do
            case p of
                Endpoint name args ret -> do
                    epDec <- mkEndpointType name args ret
                    pure [epDec]
                SubApi _name _args spec' -> mkServerFromSpec spec'


    runner    <- lift $ mkRunner spec
    serverDec <- do
        runnerName <- lift $ newName "runner_"
        ret        <- lift [t| Server $(pure $ ConT apiTypeName) |]
        let serverSigDec :: Dec
            serverSigDec = SigD serverName $ AppT (AppT ArrowT $ runner ^. typed) ret

            mkHandlerExp :: ApiPiece -> ReaderT ServerInfo Q Exp
            mkHandlerExp p = enterApiPiece p $ do
                n <- askHandlerName
                pure $ VarE n `AppE` VarE runnerName

                --enterApiPiece p $ do
                --case p of
                --    Endpoint _ _ _ -> do
                --        n <- askHandlerName
                --        pure $ VarE n `AppE` VarE runnerName
                --    SubApi _ _ _ -> do
                --        n <- askServerName
                --        pure $ VarE n `AppE` VarE runnerName

        handlers <- traverse mkHandlerExp (spec ^. typed @[ApiPiece])
        body     <- case handlers of
            []     -> error "Server contains no endpoints"
            e : es -> lift $ foldM (\b a -> [| $(pure b) :<|> $(pure a) |]) e es
        let serverFunDec :: Dec
            serverFunDec = FunD serverName [Clause [VarP runnerName] (NormalB body) []]
        pure [serverSigDec, serverFunDec]

    serverHandlerDecs <- mconcat
        <$> traverse (mkApiPieceHandler runner) (spec ^. typed @[ApiPiece])
    let bogus s = DataD
            []
            (  mkName
            $  s
            <> "________________________________________"
            <> spec
            ^. typed @GadtName
            .  typed
            .  unqualifiedString
            <> "________________________________________"
            )
            []
            Nothing
            []
            []
    pure
        $  bogus "Start"
        :  apiTypeDec
        :  serverDec
        <> epTypeDecs
        <> serverHandlerDecs
        <> [bogus "End"]
  where
    mkEndpointType
        :: ConstructorName -> ConstructorArgs -> Type -> ReaderT ServerInfo Q Dec
    mkEndpointType name args retType = do
        ty <- do
            reqBody   <- mkReqBody name args
            reqReturn <- lift $ mkReturnType retType
            middle    <- case reqBody of
                Nothing -> pure reqReturn
                Just b  -> lift [t| $(pure b) :> Post '[JSON] $(pure reqReturn) |]
            urlSegments <- mkUrlSegments name
            lift $ prependServerEndpointName urlSegments middle
        epTypeName <- askEndpointTypeName
        pure $ TySynD epTypeName [] ty


mkRunner :: ApiSpec -> Q Runner
mkRunner spec = fmap Runner [t| CmdRunner $(pure $ ConT cmdName) |]
  where
    cmdName :: Name
    cmdName = spec ^. field @"gadtName" . typed @Name

mkReturnType :: Type -> Q Type
mkReturnType = \case
    TupleT 0 -> [t| NoContent |]
    ty       -> pure ty

mkApiPieceHandler :: Runner -> ApiPiece -> ReaderT ServerInfo Q [Dec]
mkApiPieceHandler (Runner runnerType) apiPiece = enterApiPiece apiPiece $ do
    case apiPiece of
        Endpoint cName cArgs ty -> do
            let nrArgs :: Int
                nrArgs = length $ cArgs ^. typed @[Type]
            varNames       <- lift $ replicateM nrArgs (newName "arg")
            handlerRetType <- lift [t| Handler $(mkReturnType ty) |]
            handlerName    <- askHandlerName
            bodyTag        <- askBodyTag cName
            runnerName     <- lift $ newName "runner"
            let varPat :: Pat
                varPat = ConP nfName (fmap VarP varNames)

                nfName :: Name
                nfName = mkName $ "NamedFields" <> show nrArgs

                funSig :: Dec
                funSig = SigD handlerName . AppT (AppT ArrowT runnerType) $ case cArgs of
                    ConstructorArgs [] -> handlerRetType
                    ConstructorArgs as ->
                        let nfType :: Type
                            nfType = AppT (ConT nfName) (LitT bodyTag)
                        in  AppT (AppT ArrowT (foldl AppT nfType as)) handlerRetType

                funBodyBase = AppE (VarE runnerName) $ foldl
                    AppE
                    (ConE $ apiPiece ^. typed @ConstructorName . typed)
                    (fmap VarE varNames)

                funBody = case ty of
                    TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
                    _        -> pure $ funBodyBase
            funClause <- lift $ clause
                (pure (VarP runnerName) : (if nrArgs > 0 then [pure $ varPat] else []))
                (normalB [| liftIO $ $(funBody)  |])
                []
            pure [funSig, FunD handlerName [funClause]]
        -- SubApi{} -> pure [] -- Generated separately
        SubApi cName cArgs spec -> do
            -- Apply the arguments to the constructor before referencing the subserver
            varNames  <- lift $ replicateM (length (cArgs ^. typed @[Type])) (newName "arg")
            subCmdName <- lift $ newName "subCmd"
            handlerName    <- askHandlerName
            apiTypeName <- askApiTypeName
            targetApiTypeName <- enterApi spec askApiTypeName
            targetServer <- enterApi spec askServerName
            runnerName     <- lift $ newName "runner"

            funSig <- lift $ do
                returnSig <- [t| Server $(pure $ ConT targetApiTypeName) |]
                params <- case cArgs ^. typed  of
                    [] -> [t| $(pure runnerType) -> $(pure returnSig) |]
                    --[a] -> [t| $(pure runnerType) -> $(pure a) -> $(pure returnSig) |]
                    ts -> pure $ foldr1 (\b a -> AppT (AppT ArrowT b) a)
                                (runnerType : ts <> [returnSig])
                SigD handlerName <$> pure params


            funClause <- lift $ case fmap VarE varNames of
            --    [] -> clause
            --            [varP runnerName]
            --            (fmap NormalB
            --                  [e| $(varE targetApiTypeName)
            --                         $ $(pure $ VarE runnerName)
            --                         . $(pure . ConE $ cName ^. typed ) |]
            --            )
            --            []
                ts ->
                    let cmd = foldl (\b a -> AppE b a) (ConE $ cName ^. typed) ts
                     in clause
                          (varP <$>  runnerName : varNames)
                          (fmap NormalB [e| $(varE targetServer)
                                            ( $(varE runnerName). $(pure cmd))
                                            |])
                          []
            let funDef = FunD handlerName [funClause]
            pure [funSig, funDef]
        --SubApi cName cArgs spec -> do
        --    let nrArgs :: Int
        --        nrArgs = length $ cArgs ^. typed @[Type]
        --    varNames       <- lift $ replicateM nrArgs (newName "arg")
        --    serverName    <- askServerName
        --    apiTypeName <- askApiTypeName
        --    targetApiTypeName <- enterApi spec askApiTypeName
        --    runnerName     <- lift $ newName "runner"

        --    funSig <- lift $ do
        --        returnSig <- [t| Server $(pure $ ConT apiTypeName) |]
        --        params <- case cArgs ^. typed of
        --            [] -> [t| $(pure runnerType) -> $(pure returnSig) |]
        --            [a] -> [t| $(pure runnerType) -> $(pure a) -> $(pure returnSig) |]
        --            ts -> pure $ foldr1 (\b a -> AppT (AppT ArrowT b) a)
        --                        (runnerType : ts <> [returnSig])
        --        SigD serverName <$> pure params


        --    funClause <- lift $ case fmap VarE varNames of
        --        [] -> clause
        --                [varP runnerName]
        --                (fmap NormalB
        --                      [e| $(varE targetApiTypeName)
        --                             $ $(pure $ VarE runnerName)
        --                             . $(pure . ConE $ cName ^. typed) |]
        --                )
        --                []
        --        ts ->
        --            let cmd = foldl (\b a -> AppE b a) (ConE $ cName ^. typed) ts
        --             in clause
        --                  (varP <$>  runnerName : varNames)
        --                  (fmap NormalB [e| $(varE runnerName) . $(pure cmd) |])
        --                  []
        --    let funDef = FunD serverName [funClause]
        --    pure [funSig, funDef]
--
--mkSubAPiHandler :: Type -> ApiData -> ReaderT [Name] Q [Dec]
--mkSubAPiHandler runnerTy e = local (<> e ^. typed) $ do
--    runner <- lift $ newName "runner"
--
--    serverName <- askServerName
--    apiTypeName <- askApiTypeName
--    paramNames  <- traverse (const (lift $ newName "arg")) $ e ^. field @"constructorArgs"
--
--    finalSig <- lift [t| Server $(pure $ ConT apiTypeName) |]
--    params <- case e ^. field @"constructorArgs" of
--        [] -> lift [t| $(pure runnerTy) -> $(pure finalSig) |]
--        [a] -> lift [t| $(pure runnerTy) -> $(pure a) -> $(pure finalSig) |]
--        ts -> pure $ foldr1 (\b a -> AppT (AppT ArrowT b) a)
--                    (runnerTy : ts <> [finalSig])
--    funSig <- SigD serverName <$> pure params
--
--    funClause <- case fmap VarE paramNames of
--        [] -> lift $ clause
--                [varP runner]
--                (fmap NormalB
--                      [e| $(varE $ e ^. field @"subApi" . typed)
--                             $ $(pure $ VarE runner)
--                             . $(pure . ConE $ e ^. field @"gadtName") |]
--                )
--                []
--        ts ->
--            let cmd = foldl (\b a -> AppE b a) (ConE $ e ^. field @"gadtName") ts
--             in lift $ clause
--                  (varP <$>  runner : paramNames)
--                  (fmap NormalB
--                        [e| $(varE runner)
--                                . $(pure cmd)
--                        |]
--                  )
--                  []
--    let funDef = FunD serverName [funClause]
--    pure [funSig, funDef]

prependServerEndpointName :: [UrlSegment] -> Type -> Q Type
prependServerEndpointName prefix rest = do
    foldr (\a b -> [t| $(pure a) :> $b |])
          (pure rest)
          (fmap (LitT . StrTyLit . view typed) prefix)

mkReqBody :: ConstructorName -> ConstructorArgs -> ReaderT ServerInfo Q (Maybe Type)
mkReqBody name args = do
    bodyTag <- askBodyTag name
    let body = case args of
            ConstructorArgs [] -> Nothing
            ConstructorArgs ts ->
                let n = length ts
                    constructor =
                        AppT (ConT (mkName $ "NamedFields" <> show n)) (LitT bodyTag)
                in  Just $ foldl AppT constructor ts
    case body of
        Nothing -> pure Nothing
        Just b  -> Just <$> lift [t| ReqBody '[JSON] $(pure b) |]


---- | Define the servant endpoint type for non-hierarchical command constructors. E.g.
---- `BuyBook :: BookId -> Integer -> Cmd ()` will result in:
---- "BuyBook" :> ReqBody '[JSON] (BookId, Integer) -> Post '[JSON] NoContent
--cmdEndpointType :: Endpoint -> ReaderT [Name] Q Type
--cmdEndpointType p = case p of
--    SubApi   e -> epSubApiType e
--    Endpoint e -> local (<> p ^. typed) $ do
--        bodyTag <- askBodyTag
--        reqBody <- lift $ mkReqBody bodyTag (e ^. typed)
--        middle  <- case reqBody of
--            Nothing -> lift reqReturn
--            Just b  -> lift [t| $(pure b) :>  $reqReturn |]
--        lift $ prependServerEndpointName (e ^. typed) middle
--      where
--        reqReturn :: Q Type
--        reqReturn = [t| Post '[JSON] $(pure $ e ^. field @"handlerReturnType") |]
--bogusD :: String -> Dec
--bogusD s = FunD (mkName s) [Clause [] (NormalB $ VarE $ mkName s) []]
----
---- | Create a typealias for the API
---- type Api = EpA :<|> EpB :<|> EpC ...
--mkApiDec :: Name -> [Name] -> Q Dec
--mkApiDec apiName apiPieceNames = do
--    TySynD apiName [] <$> case apiPieceNames of
--        []     -> error "Server has no endpoints"
--        x : xs -> do
--            foldM f (ConT x) xs
--  where
--    f :: Type -> Name -> Q Type
--    f b a = [t| $(pure b) :<|> $(pure $ ConT a) |]
---- | Create a request body by turning multiple arguments into a NamedFieldsN
---- `BuyThing ItemKey Quantity`
---- yields:
---- `ReqBody '[JSON] (NamedFields2 "BuyThing" ItemKey Quantity)`
--mkReqBody :: TyLit -> [Type] -> Q (Maybe Type)
--mkReqBody bodyTag args = do
--    unless
--        (args == L.nub args)
--        (fail
--        $ "Each argument to the constructor must have a unique type.\
--           \\nGot request body containing: "
--        <> L.intercalate ", " (args ^.. folded . types @Name . to show)
--        )
--    let body = case args of
--            [] -> Nothing
--            ts ->
--                let n = length ts
--                    constructor =
--                        AppT (ConT (mkName $ "NamedFields" <> show n)) (LitT bodyTag)
--                in  Just $ foldl AppT constructor ts
--    case body of
--        Nothing -> pure Nothing
--        Just b  -> Just <$> [t| ReqBody '[JSON] $(pure b) |]
--
--
---- | Tag used as the symbol in in `NamedFieldsN`.
---- This field is used to gadtName the type in the OpenAPI definition.
----mkBodyTag :: EndpointData -> String
----mkBodyTag e = mconcat . (<> ["Body"]) $ prefixes & traversed . _head %~ toUpper
----  where
----    prefixes :: [String]
----    prefixes = e ^.. field @"urlPieces" . folded . to show
--
--
---- | Define the servant endpoint type for non-hierarchical query constructors. E.g.
---- `GetBook :: BookId -> Query Book` will result in:
---- "GetBook" :> Capture "BookId" BookId" :> Get '[JSON] Book
--queryEndpointType :: Endpoint -> ReaderT [Name] Q Type
--queryEndpointType p = case p of
--    SubApi   e -> epSubApiType e
--    Endpoint e -> local (<> p ^. typed) $ do
--        ty <- lift . params $ e ^. field @"constructorArgs"
--        lift $ prependServerEndpointName (e ^. typed) ty
--      where
--        params :: [Type] -> Q Type
--        params typeList = do
--            maybeType <- [t| Maybe |]
--            case typeList of
--                [] -> [t| Get '[JSON] $(pure $ e ^. field @"handlerReturnType") |]
--                AppT x t0 : ts | x == maybeType -> do
--                    tName <- case t0 of
--                        ConT n -> pure n
--                        VarT n -> pure n
--                        err -> fail $ "Expected constructor parameter, got: " <> show err
--                    let nameLit = LitT . StrTyLit $ tName ^. unqualifiedString
--                    appT
--                        (appT [t| (:>) |] [t| QueryParam $(pure nameLit) $(pure t0) |])
--                        (params ts)
--                t0 : ts -> do
--                    tName <- case t0 of
--                        ConT n -> pure n
--                        VarT n -> pure n
--                        err -> fail $ "Expected constructor parameter, got: " <> show err
--                    let nameLit = LitT . StrTyLit $ tName ^. unqualifiedString
--                    appT (appT [t| (:>) |] [t| Capture $(pure nameLit) $(pure t0) |])
--                         (params ts)
--
--
--prependServerEndpointName :: [UrlSegment] -> Type -> Q Type
--prependServerEndpointName prefix rest = do
--    foldr (\a b -> [t| $(pure a) :> $b |])
--          (pure rest)
--          (fmap (LitT . StrTyLit . view typed) prefix)
--
---- | Define a servant endpoint ending in a reference to the sub API.
---- `EditBook :: BookId -> BookCmd a -> Cmd a` will result in
---- "EditBook" :> Capture "BookId" BookId :> BookApi
--epSubApiType :: ApiData -> ReaderT [Name] Q Type
--epSubApiType e = local (<> e ^. typed @ApiSpec . typed) $ do
--    apiTypeName <- askApiTypeName
--
--    bird        <- lift [t| (:>) |]
--    let mkCapture :: Type -> Q Type
--        mkCapture t = do
--            let pName = LitT . StrTyLit $ getTypeName t
--            [t| Capture $(pure pName) $(pure t) |]
--
--        getTypeName :: Type -> String
--        getTypeName = \case
--            ConT n -> n ^. unqualifiedString
--            _      -> "typename"
--    captures <- lift $ traverse mkCapture $ e ^. field @"constructorArgs"
--    lift $ prependServerEndpointName
--        (e ^. typed)
--        (foldr1 (\a b -> AppT (AppT bird a) b) (captures <> [ConT apiTypeName]))
--
--mkEndpoints :: ApiSpec -> ReaderT [Name] Q [Dec]
--mkEndpoints s = local (<> s ^. typed) $ traverse mkEpDec (s ^. typed)
--  where
--    mkEpDec :: Endpoint -> ReaderT [Name] Q Dec
--    mkEpDec e = do
--        ty <- case s ^. typed @ServerType of
--            CmdServer   -> cmdEndpointType e
--            QueryServer -> queryEndpointType e
--        epTypeName <- local (<> e ^. typed) askApiTypeName
--        pure $ TySynD epTypeName [] ty
--
---- | Make command handlers for each endpoint
--mkHandlers :: ApiSpec -> ReaderT [Name] Q [Dec]
--mkHandlers spec = local (<> spec ^. typed) $ mconcat <$> traverse
--    mkHandler
--    (spec ^. typed @[Endpoint])
--  where
--    mkHandler :: Endpoint -> ReaderT [Name] Q [Dec]
--    mkHandler ep = do
--        runnerTy <- lift $ runnerType spec
--        case (ep, serverType spec) of
--            (Endpoint e, CmdServer  ) -> mkCmdEPHandler runnerTy e
--            (Endpoint e, QueryServer) -> mkQueryEPHandler runnerTy e
--            (SubApi   e, _          ) -> mkSubAPiHandler runnerTy e
--
--mkQueryEPHandler :: Type -> EndpointData -> ReaderT [Name] Q [Dec]
--mkQueryEPHandler runnerTy e = local (<> e ^. typed) $ do
--    varNames <- traverse (const (lift $ newName "arg")) $ e ^. field @"constructorArgs"
--    handlerRetType <- lift $ [t| Handler $(pure $ e ^. field @"handlerReturnType") |]
--    handlerName <- askHandlerName
--    let varPat = fmap VarP varNames
--        nrArgs = length @[] $ e ^. field @"constructorArgs"
--        funSig =
--            SigD handlerName
--                . AppT (AppT ArrowT runnerTy)
--                $ case e ^. field @"constructorArgs" of
--                      [] -> handlerRetType
--                      ts -> foldr (AppT . AppT ArrowT) handlerRetType ts
--        funBodyBase = AppE (VarE runner)
--            $ foldl AppE (ConE $ e ^. field @"gadtName") (fmap VarE varNames)
--
--        funBody = case e ^. field @"returnType" of
--            TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
--            _        -> pure $ funBodyBase
--    funClause <- lift $ clause
--        (pure (VarP runner) : (if nrArgs > 0 then (fmap pure varPat) else []))
--        (normalB [| liftIO $ $(funBody)  |])
--        []
--    let funDef = FunD handlerName [funClause]
--    pure [funSig, funDef]
--  where
--    runner :: Name
--    runner = mkName "runner"
--
--mkCmdEPHandler :: Type -> EndpointData -> ReaderT [Name] Q [Dec]
--mkCmdEPHandler runnerTy e = local (<> e ^. typed) $ do
--    varNames <- lift $ traverse (const $ newName "arg") $ e ^. field @"constructorArgs"
--    handlerRetType <- lift [t| Handler $(pure $ e ^. field @"handlerReturnType") |]
--    handlerName    <- askHandlerName
--    bodyTag        <- askBodyTag
--    let varPat = ConP nfName (fmap VarP varNames)
--        nfName = mkName $ "NamedFields" <> show (length varNames)
--        nrArgs = length @[] $ e ^. field @"constructorArgs"
--        funSig =
--            SigD handlerName
--                . AppT (AppT ArrowT runnerTy)
--                $ case e ^. field @"constructorArgs" of
--                      [] -> handlerRetType
--                      as ->
--                          let nfType :: Type
--                              nfType = AppT (ConT nfName) (LitT bodyTag)
--                          in  AppT (AppT ArrowT (foldl AppT nfType as)) handlerRetType
--
--        funBodyBase = AppE (VarE runner)
--            $ foldl AppE (ConE $ e ^. field @"gadtName") (fmap VarE varNames)
--
--        funBody = case e ^. field @"returnType" of
--            TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
--            _        -> pure $ funBodyBase
--    funClause <- lift $ clause
--        (pure (VarP runner) : (if nrArgs > 0 then [pure $ varPat] else []))
--        (normalB [| liftIO $ $(funBody)  |])
--        []
--    let funDef = FunD handlerName [funClause]
--    pure [funSig, funDef]
--  where
--    runner :: Name
--    runner = mkName "runner"
--
--mkSubAPiHandler :: Type -> ApiData -> ReaderT [Name] Q [Dec]
--mkSubAPiHandler runnerTy e = local (<> e ^. typed) $ do
--    runner <- lift $ newName "runner"
--
--    serverName <- askServerName
--    apiTypeName <- askApiTypeName
--    paramNames  <- traverse (const (lift $ newName "arg")) $ e ^. field @"constructorArgs"
--
--    finalSig <- lift [t| Server $(pure $ ConT apiTypeName) |]
--    params <- case e ^. field @"constructorArgs" of
--        [] -> lift [t| $(pure runnerTy) -> $(pure finalSig) |]
--        [a] -> lift [t| $(pure runnerTy) -> $(pure a) -> $(pure finalSig) |]
--        ts -> pure $ foldr1 (\b a -> AppT (AppT ArrowT b) a)
--                    (runnerTy : ts <> [finalSig])
--    funSig <- SigD serverName <$> pure params
--
--    funClause <- case fmap VarE paramNames of
--        [] -> lift $ clause
--                [varP runner]
--                (fmap NormalB
--                      [e| $(varE $ e ^. field @"subApi" . typed)
--                             $ $(pure $ VarE runner)
--                             . $(pure . ConE $ e ^. field @"gadtName") |]
--                )
--                []
--        ts ->
--            let cmd = foldl (\b a -> AppE b a) (ConE $ e ^. field @"gadtName") ts
--             in lift $ clause
--                  (varP <$>  runner : paramNames)
--                  (fmap NormalB
--                        [e| $(varE runner)
--                                . $(pure cmd)
--                        |]
--                  )
--                  []
--    let funDef = FunD serverName [funClause]
--    pure [funSig, funDef]
--
---- | The type of the CmdRunner used to execute commands
--runnerType :: ApiSpec -> Q Type
--runnerType spec = case serverType spec of
--    QueryServer -> [t| QueryRunner $(pure . ConT $ spec ^. field @"gadtName") |]
--    CmdServer   -> [t| CmdRunner $(pure . ConT $ spec ^. field @"gadtName") |]
--
---- | Create the full server and api dec
---- Assumes the handlers have already been created (using `mkHandlers`)
--mkFullServer :: ApiSpec -> ReaderT [Name] Q [Dec]
--mkFullServer spec = local (<> spec ^. typed) $ do
--    runnerT <- lift $ runnerType spec
--    let runner = mkName "runner"
--
--        handlerExp :: Monad m => Endpoint -> ReaderT [Name] m Exp
--        handlerExp = \case
--            Endpoint _ -> do
--                hName <- askHandlerName
--                pure $ VarE hName `AppE` VarE runner
--            SubApi _ -> do
--                serverName <- askServerName
--                pure $ VarE serverName `AppE` VarE runner
--
--    handlers <- traverse handlerExp $ endpoints spec
--    body     <- case handlers of
--        []     -> error "Server contains no endpoints"
--        e : es -> lift $ foldM (\b a -> [| $(pure b) :<|> $(pure a) |]) e es
--
--    -- We create the gadtName of the outer sever directly based on the GADT gadtName as this
--    -- will be referenced directly from user code.
--    let apiTypeName = spec ^. typed @Name & unqualifiedString <>~ "Api"
--        serverName =
--            spec
--                ^. typed @Name
--                &  (unqualifiedString <>~ "Server")
--                &  (unqualifiedString %~ lowerFirst)
--    apiPiecesTypeName <- traverse askApiPieceTypeName $ spec ^. typed
--
--    apiDec            <- lift $ mkApiDec apiTypeName apiPiecesTypeName
--    serverTypeDec     <- do
--        ret <- lift [t| Server $(pure $ ConT apiTypeName) |]
--        pure . SigD serverName $ AppT (AppT ArrowT runnerT) ret
--    funDec <- do
--        pure $ FunD serverName [Clause [VarP runner] (NormalB body) []]
--    pure [apiDec, serverTypeDec, funDec]
