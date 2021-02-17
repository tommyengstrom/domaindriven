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
--
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- I need to create a structure that contians both Name and [UrlSegment] to use for the
-- reader. BodyTag should have the "exposed name" but the rest of the stuff should have
-- the normal names without any mapping.
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
askBodyTag :: Monad m => ReaderT ServerInfo m TyLit
askBodyTag = do
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

-- | Create the API definition for the top level API
-- * For Endpoint this simply means referencing that API type
-- * For SubApi we add the path parameters before referencing the sub API.
--
-- Result will be something like
-- ```
-- type SomeApi = Endpoint1
--           :<|> Endpoint2
--           :<|> "CustomerKey" :> CustomerKey :> CustomerApi
mkApiTypeDec :: ApiSpec -> ReaderT ServerInfo Q Dec
mkApiTypeDec spec = do
    apiTypeName <- askApiTypeName
    let mkApiPieceType :: ApiPiece -> ReaderT ServerInfo Q Type
        mkApiPieceType p = enterApiPiece p $ case p of
            Endpoint{}           -> ConT <$> askEndpointTypeName
            SubApi cName cArgs _ -> do
                urlSegments <- mkUrlSegments cName
                n           <- askApiTypeName
                -- FIXME: This should use `fieldName` but I don't know how.
                let mkCapture :: Type -> Q Type
                    mkCapture ty =
                        let tyLit = pure $ mkLitType ty
                        in  [t| Capture $tyLit $(pure ty) |]

                    mkLitType :: Type -> Type
                    mkLitType = \case
                        VarT n' -> LitT . StrTyLit $ n' ^. unqualifiedString
                        ConT n' -> LitT . StrTyLit $ n' ^. unqualifiedString
                        _       -> LitT $ StrTyLit "unknown"

                finalType <- lift $ prependServerEndpointName urlSegments (ConT n)

                case cArgs of
                    ConstructorArgs ts -> lift $ do
                        capturesWithTitles <- do
                            captures <- traverse mkCapture ts
                            pure . mconcat $ zipWith (\a b -> [mkLitType a, b])
                                                     ts
                                                     captures
                        bird <- [t| (:>) |]
                        pure $ foldr (\a b -> bird `AppT` a `AppT` b)
                                     finalType
                                     capturesWithTitles
    epTypes <- traverse mkApiPieceType (spec ^. typed @[ApiPiece])
    case epTypes of
        []     -> error "Server contains no endpoints"
        t : ts -> do
            let fish :: Type -> Type -> Q Type
                fish b a = [t| $(pure b) :<|> $(pure a) |]
            TySynD apiTypeName [] <$> lift (foldM fish t ts)

-- | Defines the servant types for the endpoints
-- For SubApi it will trigger the full creating of the sub server with types and all
--
-- Result will be something like:
-- ```
-- type Customer_CreateEndpoint
--     = "Create"
--     :> ReqBody '[JSON] (NamedField1 "Customer_Create" Name Email)
--     :> Post '[JSON] CustomerKey
mkHandlerTypeDecs :: ApiSpec -> ReaderT ServerInfo Q [Dec]
mkHandlerTypeDecs spec = do
    fmap mconcat $ for (spec ^. typed @[ApiPiece]) $ \p -> enterApiPiece p $ do
        case p of
            Endpoint name args retType -> do
                ty <- do
                    reqBody   <- mkReqBody args
                    reqReturn <- lift $ mkReturnType retType
                    middle    <- case reqBody of
                        Nothing -> lift [t| Post '[JSON] $(pure reqReturn) |]
                        Just b  -> lift [t| $(pure b) :> Post '[JSON] $(pure reqReturn) |]
                    urlSegments <- mkUrlSegments name
                    lift $ prependServerEndpointName urlSegments middle
                epTypeName <- askEndpointTypeName
                pure [TySynD epTypeName [] ty]
            SubApi _name _args spec' -> mkServerFromSpec spec'

---- | This is the only layer of the ReaderT stack where we do not use `local` to update the
---- url segments.
mkServerFromSpec :: ApiSpec -> ReaderT ServerInfo Q [Dec]
mkServerFromSpec spec = enterApi spec $ do
    apiTypeName <- askApiTypeName
    serverName  <- askServerName
    apiTypeDec  <- mkApiTypeDec spec
    epTypeDecs  <- mkHandlerTypeDecs spec

    runner      <- lift $ mkRunner spec
    serverDec   <- do
        runnerName <- lift $ newName "runner_"
        ret        <- lift [t| Server $(pure $ ConT apiTypeName) |]
        let serverSigDec :: Dec
            serverSigDec = SigD serverName $ AppT (AppT ArrowT $ runner ^. typed) ret

            mkHandlerExp :: ApiPiece -> ReaderT ServerInfo Q Exp
            mkHandlerExp p = enterApiPiece p $ do
                n <- askHandlerName
                pure $ VarE n `AppE` VarE runnerName

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
            <> show apiTypeName
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


mkRunner :: ApiSpec -> Q Runner
mkRunner spec = fmap Runner [t| CmdRunner $(pure $ ConT cmdName) |]
  where
    cmdName :: Name
    cmdName = spec ^. field @"gadtName" . typed @Name

-- | Handles the special case of `()` being transformed into `NoContent`
mkReturnType :: Type -> Q Type
mkReturnType = \case
    TupleT 0 -> [t| NoContent |]
    ty       -> pure ty

-- | Define the servant handler for an enpoint or referens the subapi with path
-- parameters applied
mkApiPieceHandler :: Runner -> ApiPiece -> ReaderT ServerInfo Q [Dec]
mkApiPieceHandler (Runner runnerType) apiPiece = enterApiPiece apiPiece $ do
    case apiPiece of
        Endpoint _ cArgs ty -> do
            let nrArgs :: Int
                nrArgs = length $ cArgs ^. typed @[Type]
            varNames       <- lift $ replicateM nrArgs (newName "arg")
            handlerRetType <- lift [t| Handler $(mkReturnType ty) |]
            handlerName    <- askHandlerName
            bodyTag        <- askBodyTag
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
        SubApi cName cArgs spec -> do
            -- Apply the arguments to the constructor before referencing the subserver
            varNames  <- lift $ replicateM (length (cArgs ^. typed @[Type])) (newName "arg")
            handlerName    <- askHandlerName
            targetApiTypeName <- enterApi spec askApiTypeName
            targetServer <- enterApi spec askServerName
            runnerName     <- lift $ newName "runner"

            funSig <- lift $ do
                returnSig <- [t| Server $(pure $ ConT targetApiTypeName) |]
                params <- case cArgs ^. typed  of
                    [] -> [t| $(pure runnerType) -> $(pure returnSig) |]
                    ts -> pure $ foldr1 (\b a -> AppT (AppT ArrowT b) a)
                                (runnerType : ts <> [returnSig])
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

prependServerEndpointName :: [UrlSegment] -> Type -> Q Type
prependServerEndpointName prefix rest = do
    foldr (\a b -> [t| $(pure a) :> $b |])
          (pure rest)
          (fmap (LitT . StrTyLit . view typed) prefix)

mkReqBody :: ConstructorArgs -> ReaderT ServerInfo Q (Maybe Type)
mkReqBody args = do
    bodyTag <- askBodyTag
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
