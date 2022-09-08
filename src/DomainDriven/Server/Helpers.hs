module DomainDriven.Server.Helpers where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Char
import           Data.Generics.Product
import qualified Data.List                                    as L
import           DomainDriven.Internal.Class
import           DomainDriven.Server.Types
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     ( OccName(..) )
import           Prelude


runServerGenM :: ServerGenState -> ServerGenM a -> Q a
runServerGenM s m = evalStateT (unServerGenM m) s

liftQ :: Q a -> ServerGenM a
liftQ m = ServerGenM $ lift m

withLocalState :: (ServerGenState -> ServerGenState) -> ServerGenM a -> ServerGenM a
withLocalState fs m = ServerGenM $ do
    startState <- get
    modify fs
    a <- unServerGenM m
    put startState
    pure a



mkUrlSegments :: ConstructorName -> ServerGenM [UrlSegment]
mkUrlSegments n = do
    opts <- gets (view $ field @"info" . typed)
    pure
        $   n
        ^.. typed
        .   unqualifiedString
        .   to (renameConstructor opts)
        .   folded
        .   to UrlSegment



unqualifiedString :: Lens' Name String
unqualifiedString = typed @OccName . typed

-- | Turn "OhYEAH" into "ohYEAH"...
lowerFirst :: String -> String
lowerFirst = \case
    c : cs -> toLower c : cs
    []     -> []

upperFirst :: String -> String
upperFirst = \case
    c : cs -> toUpper c : cs
    []     -> []



askTypeName :: ServerGenM Name
askTypeName = do
    si <- get
    let baseName :: String
        baseName =
            si ^. field @"info" . field @"baseGadt" . typed @Name . unqualifiedString

        cNames :: [String]
        cNames =
            si
                ^.. field @"info"
                .   typed @[ConstructorName]
                .   folded
                .   typed @Name
                .   unqualifiedString
        separator :: String
        separator = si ^. field @"info" . typed @ApiOptions . field @"typenameSeparator"

    pure . mkName . L.intercalate separator $ baseName : cNames


askApiTypeName :: ServerGenM Name
askApiTypeName = (unqualifiedString <>~ "Api") <$> askTypeName

askEndpointTypeName :: ServerGenM Name
askEndpointTypeName = (unqualifiedString <>~ "Endpoint") <$> askTypeName

askServerName :: ServerGenM Name
askServerName =
    (\n -> n & unqualifiedString %~ lowerFirst & unqualifiedString <>~ "Server")
        <$> askTypeName

askHandlerName :: ServerGenM Name
askHandlerName =
    (\n -> n & unqualifiedString %~ lowerFirst & unqualifiedString <>~ "Handler")
        <$> askTypeName

askBodyTag :: ConstructorName -> ServerGenM TyLit
askBodyTag cName = do
    constructorSegments <- mkUrlSegments cName
    gadtSegment         <-
        gets (view $ field @"info" . field @"options" . field @"bodyNameBase") >>= \case
            Just n  -> pure $ UrlSegment n
            Nothing -> gets
                ( view
                $ field @"info"
                . field @"currentGadt"
                . typed
                . to nameBase
                . to UrlSegment
                )

    separator <- gets
        (view $ field @"info" . typed @ApiOptions . field @"typenameSeparator")
    pure
        .   StrTyLit
        .   L.intercalate separator
        $   (gadtSegment : constructorSegments)
        ^.. folded
        .   typed



enterApi :: ApiSpec -> ServerGenM a -> ServerGenM a
enterApi spec m = withLocalState (field @"info" %~ extendServerInfo) m
  where
    extendServerInfo :: ServerInfo -> ServerInfo
    extendServerInfo i =
        i & typed .~ spec ^. typed @ApiOptions & field @"currentGadt" .~ spec ^. typed

enterApiPiece :: ApiPiece -> ServerGenM a -> ServerGenM a
enterApiPiece p m = do
    newSegments <- mkUrlSegments (p ^. typed)
    let extendServerInfo :: ServerInfo -> ServerInfo
        extendServerInfo i =
            i
                & (typed @[UrlSegment] <>~ newSegments)
                & (typed @[ConstructorName] <>~ p ^. typed . to pure)
    withLocalState (field @"info" %~ extendServerInfo) m

--enterApi :: ApiSpec -> ServerGenM a -> ServerGenM a
--enterApi s = local extendServerInfo
--  where
--    extendServerInfo :: ServerInfo -> ServerInfo
--    extendServerInfo i =
--        i & typed .~ s ^. typed @ApiOptions & field @"currentGadt" .~ s ^. typed
--
--enterApiPiece :: ApiPiece -> ServerGenM a -> ServerGenM a
--enterApiPiece p m = do
--    newSegments <- mkUrlSegments (p ^. typed)
--    let extendServerInfo :: ServerInfo -> ServerInfo
--        extendServerInfo i =
--            i
--                & (typed @[UrlSegment] <>~ newSegments)
--                & (typed @[ConstructorName] <>~ p ^. typed . to pure)
--    local extendServerInfo m



hasJsonContentType :: HandlerSettings -> Bool
hasJsonContentType hs = case hs ^. field @"contentTypes" of
    AppT (AppT PromotedConsT (ConT n)) (SigT PromotedNilT (AppT ListT StarT)) ->
        nameBase n == "JSON"
    _ -> False
