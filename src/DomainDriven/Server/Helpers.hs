module DomainDriven.Server.Helpers where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Char
import           Data.Generics.Product
import qualified Data.List                                    as L
import           DomainDriven.Internal.Class
import           DomainDriven.Server.Types
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     ( OccName(..) )
import           Prelude


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

askBodyTag :: Monad m => ConstructorName -> ReaderT ServerInfo m TyLit
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


hasJsonContentType :: HandlerSettings -> Bool
hasJsonContentType hs = case hs ^. field @"contentTypes" of
    AppT (AppT PromotedConsT (ConT n)) (SigT PromotedNilT (AppT ListT StarT)) ->
        nameBase n == "JSON"
    _ -> False
