{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DomainDriven.Server
    ( module DomainDriven.Server
    , liftIO
    , module DomainDriven.Internal.NamedFields
    , HasFieldName(..)
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Char
import           Data.Generics.Product
import qualified Data.List                                    as L
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty                           as NE
import qualified Data.Map                                     as M
import           Data.Text                      ( Text )
import qualified Data.Text                                    as T
import           DomainDriven.Config            ( ServerConfig(..) )
import           DomainDriven.Internal.Class
import           DomainDriven.Internal.HasFieldName
                                                ( HasFieldName(..) )
import           DomainDriven.Internal.NamedFields
import           GHC.Generics                   ( Generic )
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     ( OccName(..)
                                                , VarBangType
                                                )
import           Prelude
import           Servant

data ApiSpec = ApiSpec
    { gadtName   :: GadtName -- ^ Name of the GADT representing the command
    , endpoints  :: [ApiPiece] -- ^ Endpoints created from the constructors of the GADT
    , apiOptions :: ApiOptions -- ^ The setting to use when generating part of the API
    }
    deriving (Show, Generic)

data ActionType = Mutable | Immutable deriving (Show, Eq)

data ApiPiece
    = Endpoint ConstructorName [ConstructorArg] HandlerSettings ActionType Type
    | SubApi ConstructorName [ConstructorArg] ApiSpec
    deriving (Show, Generic)

data HandlerSettings = HandlerSettings
    { contentTypes :: Type
    , verb         :: Type
    }
    deriving (Show, Generic, Eq)

newtype ConstructorName = ConstructorName Name
    deriving (Show, Generic, Eq)

newtype GadtName = GadtName Name
    deriving (Show, Generic, Eq)

newtype UrlSegment = UrlSegment String
    deriving (Show, Generic, Eq)

data ConstructorArg = ConstructorArg
    { constructorName :: Name
    , constructorType :: Type
    }
    deriving (Show, Generic, Eq)

newtype Runner = Runner Type
    deriving (Show, Generic, Eq)

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



getActionDec :: GadtName -> Q Dec
getActionDec (GadtName n) = do
    cmdType <- reify n
    let errMsg = fail "Must be GADT with two parameters, HandlerType and return type"
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


guardMethodVar :: TyVarBndr -> Q ()
guardMethodVar = \case
    KindedTV _ k -> check k
    PlainTV _    -> check StarT
  where
    check :: Type -> Q ()
    check _ = pure ()
--    check k =
--        unless (k == AppT (AppT ArrowT StarT) StarT)
--            .  fail
--            $  "Method must have be a `Verb` without the return type applied. Got: "
--            <> show k

getActionType :: Type -> Q ActionType
getActionType = \case
    AppT (AppT (AppT _ (PromotedT verbName)) _) _ -> checkVerb verbName
    ConT n -> reify n >>= \case
        TyConI (TySynD _ [] (AppT (AppT (AppT _ (PromotedT verbName)) _) _)) ->
            checkVerb verbName
        info ->
            fail
                $  "Expected method to be a Verb of a type synonym for a Verb. Got:\n"
                <> show info
    ty -> fail $ "Expected a Verb without return type applied, got: " <> show ty
  where
    checkVerb :: Name -> Q ActionType
    checkVerb n = case show n of
        "Network.HTTP.Types.Method.GET" -> pure Immutable
        _                               -> pure Mutable

guardReturnVar :: TyVarBndr -> Q ()
guardReturnVar = \case
    KindedTV _ StarT -> pure ()
    ty               -> fail $ "Return type must be a concrete type. Got: " <> show ty

getConstructors :: Dec -> Q [Con]
getConstructors = \case
    DataD _ _ [method, ret] _ cs _ -> do
        guardMethodVar method
        guardReturnVar ret
        pure cs
    d@DataD{} -> fail $ "bad data type: " <> show d
    d         -> fail $ "Expected a GADT with two parameters but got: " <> show d

unqualifiedString :: Lens' Name String
unqualifiedString = typed @OccName . typed

mkApiPiece :: ServerConfig -> Con -> Q ApiPiece
mkApiPiece cfg = \case
    -- The normal case
    RecGadtC [gadtName] bangArgs (AppT (AppT _ requestType) retType) -> do
        handlerSettings <- do
            expandedReqType <- case requestType of
                ConT n -> reify n >>= \case
                    TyConI (TySynD _ _ realType) -> pure realType
                    ty ->
                        fail
                            $  "Expected "
                            <> show n
                            <> " to be a type synonym, got: "
                            <> show ty
                (AppT _ _) -> pure requestType
                ty         -> fail $ "Expected RequestType, got: " <> show ty
            case expandedReqType of
                (AppT (AppT _RequesType contentTypes') verb') ->
                    pure $ HandlerSettings contentTypes' verb'
                ty -> fail $ "Expected RequestType, got: " <> show ty

        actionType <- getActionType $ handlerSettings ^. field @"verb"
        pure $ Endpoint (ConstructorName gadtName)
                        ((\(n, _, t) -> ConstructorArg n t) <$> bangArgs)
                        handlerSettings
                        actionType
                        retType
    GadtC [gadtName] bangArgs (AppT (AppT _ requestType) retType) -> do
        handlerSettings <- do
            expandedReqType <- case requestType of
                ConT n -> reify n >>= \case
                    TyConI (TySynD _ _ realType) -> pure realType
                    ty ->
                        fail
                            $  "Expected "
                            <> show n
                            <> " to be a type synonym, got: "
                            <> show ty
                (AppT _ _) -> pure requestType
                ty         -> fail $ "Expected RequestType, got: " <> show ty
            case expandedReqType of
                (AppT (AppT _RequesType contentTypes') verb') ->
                    pure $ HandlerSettings contentTypes' verb'
                ty -> fail $ "Expected RequestType, got: " <> show ty

        actionType <- getActionType $ handlerSettings ^. field @"verb"
        pure $ Endpoint (ConstructorName gadtName)
                        (ConstructorArg (mkName "FIXME1") . snd <$> bangArgs)
                        handlerSettings
                        actionType
                        retType
    -- When the constructor contain references to other domain models
    ForallC [method@(KindedTV methodName _), ret@(KindedTV retName _)] [] (GadtC [gadtName] bangArgs (AppT (AppT _ _) _))
        -> do
            guardMethodVar method
            guardReturnVar ret
            (args, subCmd') <- case reverse $ fmap snd bangArgs of
                AppT (AppT (ConT subCmd') (VarT methodName')) (VarT retName') : rest ->
                    do
                        unless (retName == retName')
                            $  fail
                            $  "\nSubCmd must use the return type variable of the parent."
                            <> ("\n\tExpected: " <> show retName)
                            <> ("\n\tGot: " <> show retName')
                        unless (methodName == methodName')
                            $  fail
                            $  "\nSubCmd must use the method type variable of the parent."
                            <> ("\n\tExpected: " <> show methodName)
                            <> ("\n\tGot: " <> show methodName')
                        pure (ConstructorArg (mkName "FIXME2") <$> reverse rest, subCmd')
                ty : _ ->
                    fail
                        $ "Last constructor argument must have form \
                          \`SubCmd method return`. Got: \n"
                        <> pprint ty
                [] -> fail "I thought this coulnd't happen!"

            subServerSpec <- mkServerSpec cfg (GadtName subCmd')
            pure $ SubApi (ConstructorName gadtName) args subServerSpec
    c ->
        fail
            $  "Expected a GADT constructor representing an endpoint but got:\n"
            <> pprint c
            <> show c -- FIXME: Remove once it works

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
mkServerSpec :: ServerConfig -> GadtName -> Q ApiSpec
mkServerSpec cfg n = do
    eps  <- traverse (mkApiPiece cfg) =<< getConstructors =<< getActionDec n
    opts <- getApiOptions cfg n
    pure ApiSpec { gadtName = n, endpoints = eps, apiOptions = opts }

-- | Verifies that the server do not generate overlapping paths
verifySpec :: ApiSpec -> Q ()
verifySpec _ = pure ()

------------------------------------------------------------------------------------------

-- | Generate a server with granular configuration
--
-- Expects a Map of ApiOptions generated by `DomainDriven.Config.getApiOptionsMap`
-- Due to GHC stage restrictions this cannot be generated in the same module.
--
-- Using this require you to
mkServer :: ServerConfig -> Name -> Q [Dec]
mkServer cfg (GadtName -> gadtName) = do
    spec <- mkServerSpec cfg gadtName
    opts <- getApiOptions cfg gadtName
    verifySpec spec
    let si :: ServerInfo
        si = ServerInfo { baseGadt           = spec ^. typed
                        , currentGadt        = spec ^. typed
                        , parentConstructors = []
                        , prefixSegments     = []
                        , options            = opts
                        , allFieldNames'     = (allFieldNames cfg)
                        }
    runReaderT (mkServerFromSpec spec) si

getApiOptions :: ServerConfig -> GadtName -> Q ApiOptions
getApiOptions cfg (GadtName n) = case M.lookup (nameBase n) (allApiOptions cfg) of
    Just o  -> pure o
    Nothing -> fail $ "Missing HasApiOptions instance for " <> show n


-- | Carries information regarding how the API looks at the place we're currently at.
data ServerInfo = ServerInfo
    { baseGadt           :: GadtName -- ^ Use as a prefix of all types
    , currentGadt        :: GadtName
    , parentConstructors :: [ConstructorName] -- ^ To create good names without conflict
    , prefixSegments     :: [UrlSegment] -- ^ Used to give a good name to the request body
    , options            :: ApiOptions -- ^ The current options
    , allFieldNames'     :: M.Map String Text
    }
    deriving (Show, Generic)

askTypeName :: Monad m => ReaderT ServerInfo m Name
askTypeName = do
    si <- ask
    let baseName :: String
        baseName = si ^. field @"baseGadt" . typed @Name . unqualifiedString

        cNames :: [String]
        cNames =
            si ^.. typed @[ConstructorName] . folded . typed @Name . unqualifiedString
        separator :: String
        separator = si ^. typed @ApiOptions . field @"typenameSeparator"

    pure . mkName . L.intercalate separator $ baseName : cNames

askBodyName :: Monad m => ReaderT ServerInfo m Name
askBodyName = do
    tn <- askTypeName
    pure $ mkName $ nameBase tn <> "Body"

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

askBodyTag :: ConstructorName -> ReaderT ServerInfo Q TyLit
askBodyTag cName = do
    constructorSegments <- mkUrlSegments cName
    gadtSegment <- asks (view $ field @"options" . field @"bodyNameBase") >>= \case
        Just n -> pure $ UrlSegment n
        Nothing ->
            asks (view $ field @"currentGadt" . typed . to nameBase . to UrlSegment)

    separator <- asks (view $ typed @ApiOptions . field @"typenameSeparator")
    pure
        .   StrTyLit
        .   L.intercalate separator
        $   (gadtSegment : constructorSegments)
        ^.. folded
        .   typed

enterApi :: Monad m => ApiSpec -> ReaderT ServerInfo m a -> ReaderT ServerInfo m a
enterApi s = local extendServerInfo
  where
    extendServerInfo :: ServerInfo -> ServerInfo
    extendServerInfo i =
        i & typed .~ s ^. typed @ApiOptions & field @"currentGadt" .~ s ^. typed

enterApiPiece :: Monad m => ApiPiece -> ReaderT ServerInfo m a -> ReaderT ServerInfo m a
enterApiPiece p m = do
    newSegments <- mkUrlSegments (p ^. typed)
    let extendServerInfo :: ServerInfo -> ServerInfo
        extendServerInfo i =
            i
                & (typed @[UrlSegment] <>~ newSegments)
                & (typed @[ConstructorName] <>~ p ^. typed . to pure)
    local extendServerInfo m

-- | Create the API definition for the top level API
-- * For Endpoint this simply means referencing that API type
-- * For SubApi we add the path parameters before referencing the sub API.
--
-- Result will be something like
-- ```
-- type SomeApi = Endpoint1
--           :<|> Endpoint2
--           :<|> "CustomerKey" :> CustomerKey :> CustomerApi
mkApiTypeDecs :: ApiSpec -> ReaderT ServerInfo Q [Dec]
mkApiTypeDecs spec = do
    apiTypeName <- askApiTypeName
    epTypes     <- traverse (mkEndpointApiType $ spec ^. typed @ApiOptions)
                            (spec ^. typed @[ApiPiece])
    topLevelDec <- case reverse epTypes of -- :<|> is right associative
        []     -> fail "Server contains no endpoints"
        t : ts -> do
            let fish :: Type -> Type -> Q Type
                fish b a = [t| $(pure a) :<|> $(pure b) |]
            TySynD apiTypeName [] <$> lift (foldM fish t ts)
    handlerDecs <- mconcat <$> traverse mkHandlerTypeDec (spec ^. typed @[ApiPiece])
    pure $ topLevelDec : handlerDecs

-- | Create endpoint types to be referenced in the API
-- * For Endpoint this is just a reference to the handler type
-- * For SubApi we apply the path parameters before referencing the SubApi
mkEndpointApiType :: ApiOptions -> ApiPiece -> ReaderT ServerInfo Q Type
mkEndpointApiType opts p = enterApiPiece p $ case p of
    Endpoint{}           -> ConT <$> askEndpointTypeName
    SubApi cName cArgs _ -> do
        urlSegments <- mkUrlSegments cName
        n           <- askApiTypeName
        fieldNames  <- asks allFieldNames'
        let mkCapture :: Type -> Q Type
            mkCapture ty =
                let tyLit = pure $ mkLitType id ty in [t| Capture $tyLit $(pure ty) |]

            useFieldname :: String -> String
            useFieldname n' = maybe n' T.unpack $ M.lookup n' fieldNames

            mkLitType :: (String -> String) -> Type -> Type
            mkLitType f = \case
                VarT n' -> LitT . StrTyLit . f . useFieldname $ n' ^. unqualifiedString
                ConT n' -> LitT . StrTyLit . f . useFieldname $ n' ^. unqualifiedString
                _       -> LitT $ StrTyLit "unknown"

        finalType <- lift $ prependServerEndpointName urlSegments (ConT n)

        case view typed <$> cArgs of
            ts -> lift $ do
                capturesWithTitles <- do
                    captures <- traverse mkCapture ts
                    pure . mconcat $ zipWith
                        (\a b -> [mkLitType (opts ^. field @"renamePathParams") a, b])
                        ts
                        captures
                bird <- [t| (:>) |]
                pure $ foldr (\a b -> bird `AppT` a `AppT` b) finalType capturesWithTitles

-- | Defines the servant types for the endpoints
-- For SubApi it will trigger the full creating of the sub server with types and all
--
-- Result will be something like:
-- ```
-- type Customer_CreateEndpoint
--     = "Create"
--     :> ReqBody '[JSON] (NamedField1 "Customer_Create" Name Email)
--     :> Post '[JSON] CustomerKey
mkHandlerTypeDec :: ApiPiece -> ReaderT ServerInfo Q [Dec]
mkHandlerTypeDec p = enterApiPiece p $ do
    case p of
        Endpoint name args hs Immutable retType -> do
            -- Get endpoint will use query parameters
            ty <- do
                queryParams <- mkQueryParams args
                reqReturn   <- lift $ mkVerb hs <$> mkReturnType retType
                bird        <- lift [t| (:>) |]
                let stuff = foldr1 joinUrlParts $ queryParams <> [reqReturn]
                    joinUrlParts :: Type -> Type -> Type
                    joinUrlParts a b = AppT (AppT bird a) b
                urlSegments <- mkUrlSegments name
                lift $ prependServerEndpointName urlSegments stuff
            epTypeName <- askEndpointTypeName
            pure [TySynD epTypeName [] ty]
        Endpoint name args hs Mutable retType -> do
            -- Non-get endpoints use a request body
            case args of
                [] -> do
                    reqReturn   <- lift $ mkReturnType retType
                    middle      <- lift [t|  $(pure $ mkVerb hs reqReturn) |]
                    urlSegments <- mkUrlSegments name
                    ty          <- lift $ prependServerEndpointName urlSegments middle
                    epTypeName  <- askEndpointTypeName
                    pure $ TySynD epTypeName [] ty : []
                a : as -> do
                    (reqBody, decs) <- mkReqBody hs (a :| as)
                    reqReturn       <- lift $ mkReturnType retType
                    middle <- lift [t| $(pure reqBody) :> $(pure $ mkVerb hs reqReturn) |]
                    urlSegments     <- mkUrlSegments name
                    ty              <- lift $ prependServerEndpointName urlSegments middle
                    epTypeName      <- askEndpointTypeName
                    pure $ TySynD epTypeName [] ty : decs
        SubApi _name _args spec' -> mkServerFromSpec spec'


mkQueryParams :: [ConstructorArg] -> ReaderT ServerInfo Q [Type]
mkQueryParams args = do
    may <- lift [t| Maybe |] -- Maybe parameters are optional, others required
    let mkTyName :: Name -> Q Type
        mkTyName n = pure . LitT . StrTyLit $ n ^. unqualifiedString
    flip traverse args $ \case
        ConstructorArg _ ty@(AppT may'  (ConT n)) | may' == may ->
            lift
                [t| QueryParam' '[Optional, Servant.Strict] $(mkTyName n) $(pure ty) |]
        ConstructorArg _ ty@(ConT n) ->
            lift
                [t| QueryParam' '[Required, Servant.Strict] $(mkTyName n) $(pure ty) |]
        ConstructorArg _ ty -> fail $ "Expected type ConT, got: " <> show ty

mkVerb :: HandlerSettings -> Type -> Type
mkVerb (HandlerSettings _ verb) ret = verb `AppT` ret
-- | Declare then handlers for the API
--
mkServerDec :: ApiSpec -> ReaderT ServerInfo Q [Dec]
mkServerDec spec = do
    apiTypeName <- askApiTypeName
    serverName  <- askServerName

    runner      <- lift $ mkRunner spec
    let runnerName = mkName "runner"
    ret <- lift [t| Server $(pure $ ConT apiTypeName) |]
    let serverSigDec :: Dec
        serverSigDec = SigD serverName $ AppT (AppT ArrowT $ runner ^. typed) ret

        mkHandlerExp :: ApiPiece -> ReaderT ServerInfo Q Exp
        mkHandlerExp p = enterApiPiece p $ do
            n <- askHandlerName
            pure $ VarE n `AppE` VarE runnerName

    handlers <- traverse mkHandlerExp (spec ^. typed @[ApiPiece])
    body     <- case reverse handlers of -- :<|> is right associative
        []     -> fail "Server contains no endpoints"
        e : es -> lift $ foldM (\b a -> [| $(pure a) :<|> $(pure b) |]) e es
    let serverFunDec :: Dec
        serverFunDec = FunD serverName [Clause [VarP runnerName] (NormalB body) []]
    serverHandlerDecs <- mconcat
        <$> traverse (mkApiPieceHandler runner) (spec ^. typed @[ApiPiece])

    pure $ serverSigDec : serverFunDec : serverHandlerDecs

mkRunner :: ApiSpec -> Q Runner
mkRunner spec = do
    Runner <$> [t| ActionRunner $(pure cmdType)  |]
  where
    cmdType :: Type
    cmdType = spec ^. field @"gadtName" . typed @Name . to ConT

-- | Define the servant handler for an enpoint or referens the subapi with path
-- parameters applied
mkApiPieceHandler :: Runner -> ApiPiece -> ReaderT ServerInfo Q [Dec]
mkApiPieceHandler (Runner runnerType) apiPiece = enterApiPiece apiPiece $ do
    case apiPiece of
        Endpoint _ (fmap (view typed) -> cArgs) _ Immutable ty -> do
            let nrArgs :: Int
                nrArgs = length $ cArgs ^. typed @[Type]
            varNames       <- lift $ replicateM nrArgs (newName "arg")
            handlerRetType <- lift [t| Handler $(mkReturnType ty) |]
            handlerName    <- askHandlerName
            runnerName     <- lift $ newName "runner"
            let funSig :: Dec
                funSig = SigD handlerName . AppT (AppT ArrowT runnerType) $ case cArgs of
                    args -> foldr1 (\a b -> AppT (AppT ArrowT a) b) (args <> [handlerRetType])

                funBodyBase = AppE (VarE runnerName) $ foldl
                    AppE
                    (ConE $ apiPiece ^. typed @ConstructorName . typed)
                    (fmap VarE varNames)

                funBody = case ty of
                    TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
                    _        -> pure $ funBodyBase
            funClause <- lift $ clause
                (fmap (pure . VarP) (runnerName:varNames ))
                (normalB [| liftIO $ $(funBody)  |])
                []
            pure [funSig, FunD handlerName [funClause]]
        Endpoint cName cArgs hs Mutable ty | hasJsonContentType hs -> do
            let nrArgs :: Int
                nrArgs = length cArgs
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
                    [] -> handlerRetType
                    (fmap (view typed) -> as) ->
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
        Endpoint _cName cArgs _hs Mutable ty -> do
            -- FIXME: For non-JSON request bodies we support only one argument.
            -- Combining JSON with other content types do not work properly at this point.
            -- It could probably be fixed by adding MimeRender instances to NamedField1
            -- that just uses the underlying MimeRender.
            let nrArgs :: Int
                nrArgs = length cArgs
            unless (nrArgs < 2) (fail "Only one argument is supported for non-JSON request bodies")
            varName       <- lift $ newName "arg"
            handlerRetType <- lift [t| Handler $(mkReturnType ty) |]
            handlerName    <- askHandlerName
            runnerName     <- lift $ newName "runner"
            let varPat :: Pat
                varPat = VarP varName

                funSig :: Dec
                funSig = SigD handlerName . AppT (AppT ArrowT runnerType) $ case cArgs of
                    [] -> handlerRetType
                    (ConstructorArg _ ty':_) -> AppT (AppT ArrowT ty') handlerRetType

                funBodyBase = AppE (VarE runnerName) $
                    AppE
                    (ConE $ apiPiece ^. typed @ConstructorName . typed)
                    (VarE varName)

                funBody = case ty of
                    TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
                    _        -> pure $ funBodyBase
            funClause <- lift $ clause
                (pure (VarP runnerName) : (if nrArgs > 0 then [pure $ varPat] else []))
                (normalB [| liftIO $ $(funBody)  |])
                []
            pure [funSig, FunD handlerName [funClause]]
        SubApi cName cArgs spec -> do
            -- Apply the arguments to the constructor before referencing the subserver
            varNames  <- lift $ replicateM (length cArgs) (newName "arg")
            handlerName    <- askHandlerName
            targetApiTypeName <- enterApi spec askApiTypeName
            targetServer <- enterApi spec askServerName
            runnerName     <- lift $ newName "runner"

            funSig <- lift $ do
                returnSig <- [t| Server $(pure $ ConT targetApiTypeName) |]
                params <- case cArgs  of
                    [] -> [t| $(pure runnerType) -> $(pure returnSig) |]
                    ts -> pure $ foldr1 (\b a -> AppT (AppT ArrowT b) a)
                                (runnerType : (ts ^.. folded . field @"constructorType") <> [returnSig])
                SigD handlerName <$> pure params


            funClause <- lift $ do
                let cmd = foldl (\b a -> AppE b a)
                                (ConE $ cName ^. typed)
                                (fmap VarE varNames)
                 in clause
                      (varP <$>  runnerName : varNames)
                      (fmap NormalB [e| $(varE targetServer)
                                        ( $(varE runnerName). $(pure cmd))
                                        |])
                      []
            let funDef = FunD handlerName [funClause]
            pure [funSig, funDef]
---- | This is the only layer of the ReaderT stack where we do not use `local` to update the
---- url segments.
mkServerFromSpec :: ApiSpec -> ReaderT ServerInfo Q [Dec]
mkServerFromSpec spec = enterApi spec $ do
    apiTypeDecs <- mkApiTypeDecs spec
    serverDecs  <- mkServerDec spec
    pure $ apiTypeDecs <> serverDecs


-- | Handles the special case of `()` being transformed into `NoContent`
mkReturnType :: Type -> Q Type
mkReturnType = \case
    TupleT 0 -> [t| NoContent |]
    ty       -> pure ty

prependServerEndpointName :: [UrlSegment] -> Type -> Q Type
prependServerEndpointName prefix rest = do
    foldr (\a b -> [t| $(pure a) :> $b |])
          (pure rest)
          (fmap (LitT . StrTyLit . view typed) prefix)

mkReqBody
    :: HandlerSettings -> NonEmpty ConstructorArg -> ReaderT ServerInfo Q (Type, [Dec])
mkReqBody hs args = if hasJsonContentType hs
--    then do
--        bodyTag <- askBodyTag name
--        let
--            body = case args of
--                [] -> Nothing
--                (fmap (view $ field @"constructorType") -> ts) ->
--                    let n           = length ts
--                        constructor = AppT (ConT (mkName $ "NamedFields" <> show n))
--                                           (LitT bodyTag)
--                    in  Just $ foldl AppT constructor ts
--        case body of
--            Nothing -> pure (Nothing, [])
--            Just b  -> do
--                ty <- lift [t| ReqBody '[JSON] $(pure b) |]
--                pure (Just ty, [])
    then do
        -- bodyTag <- askBodyTag name
        bodyName <- askBodyName
        decs <- lift $ mkRecord bodyName args
        pure (ConT bodyName, decs)
        --case args of
        --    [] -> pure (Nothing, [])
        --    (fmap (view $ field @"constructorType") -> ts) -> do
        --        let n           = length ts
        --            constructor = AppT (ConT (mkName $ "NamedFields" <> show n))
        --                               (LitT bodyTag)
        --            body = foldl AppT constructor ts
        --        ty <- lift [t| ReqBody '[JSON] $(pure body) |]
        --        pure (Just ty, [])
    else case args of
            ca :| []  -> do
                let b = ca ^. field @"constructorType"
                ty <- lift
                    [t| ReqBody $(pure $ hs ^. field @"contentTypes") $(pure b) |]
                pure (ty, [])
            _ ->
                    fail "Multiple arguments are only supported for JSON content"

mkRecord :: Name -> NonEmpty ConstructorArg -> Q [Dec]
mkRecord name (NE.toList -> cArgs) = do
    pure [DataD [] name [] Nothing [RecC name recFields] deriv]
  where
    recFields :: [VarBangType]
    recFields = fmap
        (\(ConstructorArg n ty) ->
            (mkName $ nameBase n, Bang NoSourceUnpackedness SourceStrict, ty)
        )
        cArgs

    deriv :: [DerivClause]
    deriv =
        [ DerivClause (Just StockStrategy)    [ConT ''Show, ConT ''Generic]
        , DerivClause (Just AnyclassStrategy) [ConT ''FromJSON, ConT ''ToJSON]
        ]

hasJsonContentType :: HandlerSettings -> Bool
hasJsonContentType hs = case hs ^. field @"contentTypes" of
    AppT (AppT PromotedConsT (ConT n)) (SigT PromotedNilT (AppT ListT StarT)) ->
        nameBase n == "JSON"
    _ -> False
