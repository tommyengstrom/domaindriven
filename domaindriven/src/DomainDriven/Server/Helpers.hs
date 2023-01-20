module DomainDriven.Server.Helpers where

import Control.Monad
import Control.Monad.State
import Data.Generics.Product
import qualified Data.List as L
import DomainDriven.Internal.Text
import DomainDriven.Server.Types
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (OccName (..))
import Lens.Micro
import Prelude

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

mkUrlSegment :: ConstructorName -> ServerGenM UrlSegment
mkUrlSegment n = do
    opts <- gets (^. field @"info" . field @"options")
    pure $
        n
            ^. typed
                . unqualifiedString
                . to (opts ^. field @"renameConstructor")
                . to UrlSegment

unqualifiedString :: Lens' Name String
unqualifiedString = typed @OccName . typed

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
                    . typed @[ConstructorName]
                    . folded
                    . typed @Name
                    . unqualifiedString
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
    constructorSegment <- mkUrlSegment cName
    gadtSegment <-
        gets (^. field @"info" . field @"options" . field @"bodyNameBase") >>= \case
            Just n -> pure $ UrlSegment n
            Nothing ->
                gets
                    ( ^.
                        field @"info"
                            . field @"currentGadt"
                            . typed
                            . to nameBase
                            . to UrlSegment
                    )

    separator <- gets (^. field @"info" . typed @ApiOptions . field @"typenameSeparator")
    pure
        . StrTyLit
        . L.intercalate separator
        $ (gadtSegment : [constructorSegment])
            ^.. folded
                . typed

enterApi :: ApiSpec -> ServerGenM a -> ServerGenM a
enterApi spec m = withLocalState (field @"info" %~ extendServerInfo) m
  where
    extendServerInfo :: ServerInfo -> ServerInfo
    extendServerInfo i =
        i & typed .~ spec ^. typed @ApiOptions & field @"currentGadt" .~ spec ^. typed

enterApiPiece :: ApiPiece -> ServerGenM a -> ServerGenM a
enterApiPiece p m = do
    newSegment <- mkUrlSegment (p ^. typed @ConstructorName)
    let extendServerInfo :: ServerInfo -> ServerInfo
        extendServerInfo i =
            i
                & (typed @[UrlSegment] <>~ [newSegment])
                & (typed @[ConstructorName] <>~ p ^. typed . to pure)
    withLocalState (field @"info" %~ extendServerInfo) m

hasJsonContentType :: HandlerSettings -> Bool
hasJsonContentType hs = case hs ^. field @"contentTypes" of
    AppT (AppT PromotedConsT (ConT n)) (SigT PromotedNilT (AppT ListT StarT)) ->
        nameBase n == "JSON"
    _ -> False
