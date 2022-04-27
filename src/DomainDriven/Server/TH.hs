{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DomainDriven.Server.TH where


import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch            ( MonadThrow(..) )
import           Control.Monad.Reader
import           Data.Char
import           Data.Generics.Product
import qualified Data.Map                                     as M
import qualified Data.Text                                    as T
import           DomainDriven.Config            ( ServerConfig(..) )
import           DomainDriven.Internal.Class
import           DomainDriven.Server.Helpers
import           DomainDriven.Server.Types
import           Language.Haskell.TH
import           Prelude
import           Servant
import           UnliftIO                       ( MonadUnliftIO(..) )


getActionDec :: GadtName -> Q Dec
getActionDec (GadtName n) = do
    cmdType <- reify n
    let errMsg = fail "Must be GADT with two parameters, HandlerType and return type"
    case cmdType of
        TyConI dec   -> pure dec
        ClassI{}     -> errMsg
        ClassOpI{}   -> errMsg
        FamilyI{}    -> errMsg
        PrimTyConI{} -> errMsg
        DataConI{}   -> errMsg
        PatSynI{}    -> errMsg
        VarI{}       -> errMsg
        TyVarI{}     -> errMsg


guardMethodVar :: TyVarBndr flag -> Q ()
guardMethodVar = \case
    KindedTV _ _ k -> check k
    PlainTV _ _    -> check StarT
  where
    check :: Type -> Q ()
    check _ = pure ()

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

guardReturnVar :: Show flag => TyVarBndr flag -> Q ()
guardReturnVar = \case
    KindedTV _ _ StarT -> pure ()
    ty                 -> fail $ "Return type must be a concrete type. Got: " <> show ty

getConstructors :: Dec -> Q [Con]
getConstructors = \case
    DataD _ _ [method, ret] _ cs _ -> do
        guardMethodVar method
        guardReturnVar ret
        pure cs
    d@DataD{} -> fail $ "bad data type: " <> show d
    d         -> fail $ "Expected a GADT with two parameters but got: " <> show d

mkApiPiece :: ServerConfig -> Con -> Q ApiPiece
mkApiPiece cfg = \case
    -- The normal case
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
                        (ConstructorArgs $ fmap snd bangArgs)
                        handlerSettings
                        actionType
                        (EpReturnType retType)
    -- When the constructor contain references to other domain models
    ForallC [method@(KindedTV methodName _ _), ret@(KindedTV retName _ _)] [] (GadtC [gadtName] bangArgs (AppT (AppT _ _) _))
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
                        pure (reverse rest, subCmd')
                ty : _ ->
                    fail
                        $ "Last constructor argument must have form \
                          \`SubCmd method return`. Got: \n"
                        <> pprint ty
                [] -> fail "I thought this coulnd't happen!"

            subServerSpec <- mkServerSpec cfg (GadtName subCmd')
            pure $ SubApi (ConstructorName gadtName) (ConstructorArgs args) subServerSpec
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
-- Using this require you to enable template haskell
-- $(mkServer  config ''MyAction)
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
    epTypes     <- traverse mkEndpointApiType (spec ^. typed @[ApiPiece])
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
mkEndpointApiType :: ApiPiece -> ReaderT ServerInfo Q Type
mkEndpointApiType p = enterApiPiece p $ case p of
    Endpoint{}           -> ConT <$> askEndpointTypeName
    SubApi cName cArgs _ -> do
        urlSegments <- mkUrlSegments cName
        n           <- askApiTypeName
        fieldNames  <- asks allFieldNames'
        finalType   <- lift $ prependServerEndpointName urlSegments (ConT n)

        params      <- mkQueryParams cArgs
        bird        <- lift [t| (:>) |]
        pure $ foldr (\a b -> bird `AppT` a `AppT` b) finalType params

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
                let reqReturn = mkVerb hs $ mkReturnType retType
                bird <- lift [t| (:>) |]
                let stuff = foldr1 joinUrlParts $ queryParams <> [reqReturn]
                    joinUrlParts :: Type -> Type -> Type
                    joinUrlParts a b = AppT (AppT bird a) b
                urlSegments <- mkUrlSegments name
                lift $ prependServerEndpointName urlSegments stuff
            epTypeName <- askEndpointTypeName
            pure [TySynD epTypeName [] ty]
        Endpoint name args hs Mutable retType -> do
            -- Non-get endpoints use a request body
            ty <- do
                reqBody <- mkReqBody hs name args
                let reqReturn = mkReturnType retType
                middle <- case reqBody of
                    Nothing -> pure $ mkVerb hs reqReturn
                    Just b  -> lift [t| $(pure b) :> $(pure $ mkVerb hs reqReturn) |]
                urlSegments <- mkUrlSegments name
                lift $ prependServerEndpointName urlSegments middle
            epTypeName <- askEndpointTypeName
            pure [TySynD epTypeName [] ty]
        SubApi _name _args spec' -> mkServerFromSpec spec'


mkQueryParams :: ConstructorArgs -> ReaderT ServerInfo Q [Type]
mkQueryParams (ConstructorArgs args) = do
    may <- lift [t| Maybe |] -- Maybe parameters are optional, others required
    let mkTyName :: Name -> Q Type
        mkTyName n = pure . LitT . StrTyLit $ n ^. unqualifiedString
    flip traverse args $ \case
        (AppT may'  ty@(ConT n)) | may' == may ->
            lift
                [t| QueryParam' '[Optional, Servant.Strict] $(mkTyName n) $(pure ty) |]
        ty@(ConT n) ->
            lift
                [t| QueryParam' '[Required, Servant.Strict] $(mkTyName n) $(pure ty) |]
        ty -> fail $ "Expected type ConT, got: " <> show ty

mkVerb :: HandlerSettings -> Type -> Type
mkVerb (HandlerSettings _ verb) ret = verb `AppT` ret
-- | Declare then handlers for the API

mkServerDec :: ApiSpec -> ReaderT ServerInfo Q [Dec]
mkServerDec spec = do
    apiTypeName <- askApiTypeName
    serverName  <- askServerName

    let runnerName = mkName "runner"

    let gadtType :: GadtType
        gadtType = GadtType $ spec ^. field @"gadtName" . typed @Name . to ConT
    serverType <- lift
        [t| forall m. (MonadThrow m, MonadUnliftIO m)
                  => ActionRunner m $(pure $ unGadtType gadtType)
                  -> ServerT $(pure $ ConT apiTypeName) m
          |]

    -- ret <- lift [t| Server $(pure $ ConT apiTypeName) |]
    let serverSigDec :: Dec
        serverSigDec = SigD serverName serverType

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
        <$> traverse (mkApiPieceHandler gadtType) (spec ^. typed @[ApiPiece])

    pure $ serverSigDec : serverFunDec : serverHandlerDecs




withForall :: Type -> Type
withForall = ForallT
    [PlainTV runnerMonadName SpecifiedSpec]
    [ ConT ''MonadUnliftIO `AppT` VarT runnerMonadName
    , ConT ''MonadThrow `AppT` VarT runnerMonadName
    ]

actionRunner :: Type -> Type
actionRunner runnerGADT =
    ConT ''ActionRunner `AppT` VarT runnerMonadName `AppT` runnerGADT

runnerMonadName :: Name
runnerMonadName = mkName "m"

mkNamedFieldsType
    :: ConstructorName -> ConstructorArgs -> ReaderT ServerInfo Q (Maybe Type)
mkNamedFieldsType cName = \case
    ConstructorArgs []   -> pure Nothing
    ConstructorArgs args -> do
        bodyTag <- askBodyTag cName

        let nfType :: Type
            nfType = AppT (ConT nfName) (LitT bodyTag)

            nfName :: Name
            nfName = mkName $ "NamedFields" <> show (length args)
        pure . Just $ foldl AppT nfType args

mkQueryHandlerSignature :: GadtType -> ConstructorArgs -> EpReturnType -> Type
mkQueryHandlerSignature (GadtType actionType) (ConstructorArgs args) (EpReturnType retType)
    = withForall $ mkFunction $ actionRunner actionType : args <> [ret]
  where
    ret :: Type
    ret = AppT (VarT runnerMonadName) retType


-- | Makes command handler, e.g.
--  counterCmd_AddToCounterHandler ::
--    ActionRunner m CounterCmd -> NamedFields1 "CounterCmd_AddToCounter" Int -> m Int
mkCmdHandlerSignature
    :: Type
    -> ConstructorName
    -> ConstructorArgs
    -> EpReturnType
    -> ReaderT ServerInfo Q Type
mkCmdHandlerSignature actionType cName cArgs (EpReturnType retType) = do
    nfArgs <- mkNamedFieldsType cName cArgs
    pure
        $  withForall
        $  mkFunction
        $  [actionRunner actionType]
        <> maybe [] pure nfArgs
        <> [ret]
  where
    ret :: Type
    ret = AppT (VarT runnerMonadName) $ case retType of
        TupleT 0 -> ConT ''NoContent
        ty       -> ty


mkFunction :: [Type] -> Type
mkFunction = foldr1 (\a b -> AppT (AppT ArrowT a) b)

-- | Define the servant handler for an enpoint or referens the subapi with path
-- parameters applied
mkApiPieceHandler :: GadtType -> ApiPiece -> ReaderT ServerInfo Q [Dec]
mkApiPieceHandler gadtType@(GadtType actionType') apiPiece = enterApiPiece apiPiece $  do
    case apiPiece of
        Endpoint _cName cArgs _hs Immutable ty -> do
            let nrArgs :: Int
                nrArgs = length $ cArgs ^. typed @[Type]
            varNames       <- lift $ replicateM nrArgs (newName "arg")
            handlerName    <- askHandlerName
            runnerName     <- lift $ newName "runner"

            let funSig :: Dec
                funSig = SigD handlerName
                 $ mkQueryHandlerSignature gadtType cArgs ty

                funBodyBase = AppE (VarE runnerName) $ foldl
                    AppE
                    (ConE $ apiPiece ^. typed @ConstructorName . typed)
                    (fmap VarE varNames)

                funBody = case ty ^. typed of
                    TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
                    _        -> pure $ funBodyBase
            funClause <- lift $ clause
                (fmap (pure . VarP) (runnerName:varNames ))
                (normalB [|  $(funBody)  |])
                []
            pure [funSig, FunD handlerName [funClause]]
        Endpoint cName cArgs hs Mutable ty | hasJsonContentType hs -> do
            let nrArgs :: Int
                nrArgs = length $ cArgs ^. typed @[Type]
            varNames       <- lift $ replicateM nrArgs (newName "arg")
            handlerName    <- askHandlerName
            runnerName     <- lift $ newName "runner"
            let varPat :: Pat
                varPat = ConP nfName [] (fmap VarP varNames)

                nfName :: Name
                nfName = mkName $ "NamedFields" <> show nrArgs

            funSig <- SigD handlerName <$> mkCmdHandlerSignature actionType' cName cArgs ty

            let funBodyBase = AppE (VarE runnerName) $ foldl
                    AppE
                    (ConE $ apiPiece ^. typed @ConstructorName . typed)
                    (fmap VarE varNames)

                funBody = case ty ^. typed of
                    TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
                    _        -> pure $ funBodyBase
            funClause <- lift $ clause
                (pure (VarP runnerName) : (if nrArgs > 0 then [pure $ varPat] else []))
                (normalB [|  $(funBody)  |])
                []
            pure [funSig, FunD handlerName [funClause]]
        Endpoint _cName cArgs _hs Mutable ty -> do
            -- FIXME: For non-JSON request bodies we support only one argument.
            -- Combining JSON with other content types do not work properly at this point.
            -- It could probably be fixed by adding MimeRender instances to NamedField1
            -- that just uses the underlying MimeRender.
            let nrArgs :: Int
                nrArgs = length $ cArgs ^. typed @[Type]
            unless (nrArgs < 2) $
                fail "Only one argument is supported for non-JSON request bodies"
            varName       <- lift $ newName "arg"
            handlerName    <- askHandlerName
            runnerName     <- lift $ newName "runner"
            let varPat :: Pat
                varPat = VarP varName

            let funSig :: Dec
                funSig = SigD handlerName $ mkQueryHandlerSignature gadtType cArgs ty

                funBodyBase = AppE (VarE runnerName) $
                    AppE
                    (ConE $ apiPiece ^. typed @ConstructorName . typed)
                    (VarE varName)

                funBody = case ty ^. typed of
                    TupleT 0 -> [| fmap (const NoContent) $(pure funBodyBase) |]
                    _        -> pure $ funBodyBase
            funClause <- lift $ clause
                (pure (VarP runnerName) : (if nrArgs > 0 then [pure $ varPat] else []))
                (normalB [|  $(funBody)  |])
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
                let params = withForall $ mkFunction $
                        [ actionRunner actionType']
                        <> cArgs ^. typed @[Type]
                        <> [ConT ''ServerT
                            `AppT` (ConT targetApiTypeName)
                            `AppT` (VarT runnerMonadName)]
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
mkReturnType :: EpReturnType -> Type
mkReturnType (EpReturnType ty) = case ty of
    TupleT 0 -> ConT ''NoContent
    _        -> ty

prependServerEndpointName :: [UrlSegment] -> Type -> Q Type
prependServerEndpointName prefix rest = do
    foldr (\a b -> [t| $(pure a) :> $b |])
          (pure rest)
          (fmap (LitT . StrTyLit . view typed) prefix)

mkReqBody
    :: HandlerSettings
    -> ConstructorName
    -> ConstructorArgs
    -> ReaderT ServerInfo Q (Maybe Type)
mkReqBody hs name args = if hasJsonContentType hs
    then do
        bodyTag <- askBodyTag name
        let
            body = case args of
                ConstructorArgs [] -> Nothing
                ConstructorArgs ts ->
                    let n           = length ts
                        constructor = AppT (ConT (mkName $ "NamedFields" <> show n))
                                           (LitT bodyTag)
                    in  Just $ foldl AppT constructor ts
        case body of
            Nothing -> pure Nothing
            Just b  -> Just <$> lift [t| ReqBody '[JSON] $(pure b) |]
    else do
        let
            body = case args of
                ConstructorArgs []  -> Nothing
                ConstructorArgs [t] -> Just t
                ConstructorArgs _ ->
                    fail "Multiple arguments are only supported for JSON content"
        case body of
            Nothing -> pure Nothing
            Just b ->
                Just <$> lift
                    [t| ReqBody $(pure $ hs ^. field @"contentTypes") $(pure b) |]
