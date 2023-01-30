module DomainDriven.Server.Types where

import Control.Monad.State
import Data.Function (on)
import Data.Generics.Product
import Data.List qualified as L
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Lens.Micro ((^.))
import Prelude

-- Contains infromatiotion of how the API should look, gathered from the Action GADT.
data ApiSpec = ApiSpec
    { gadtName :: GadtName
    , gadtType :: GadtType
    -- ^ Name of the GADT representing the command
    , allVarBindings :: VarBindings
    , endpoints :: [ApiPiece]
    -- ^ Endpoints created from the constructors of the GADT
    , options :: ApiOptions
    -- ^ The setting to use when generating part of the API
    }
    deriving (Show, Generic)

data VarBindings = VarBindings
    { paramPart :: Name
    , method :: Name
    , return :: Name
    , extra :: [TyVarBndr ()]
    }
    deriving (Show, Generic, Eq)

data ApiOptions = ApiOptions
    { renameConstructor :: String -> String
    , typenameSeparator :: String
    , bodyNameBase :: Maybe String
    }
    deriving (Generic)

defaultApiOptions :: ApiOptions
defaultApiOptions =
    ApiOptions
        { renameConstructor =
            \s -> case L.splitAt (on (-) L.length s "Action") s of
                (x, "Action") -> x
                _ -> s
        , typenameSeparator = "_"
        , bodyNameBase = Nothing
        }

instance Show ApiOptions where
    show o =
        "ApiOptions {renameConstructor = ***, typenameSeparator = \""
            <> o
                ^. field @"typenameSeparator"
            <> "\"}"

data Mutability
    = Mutable
    | Immutable
    deriving (Show, Eq)

data ApiPiece
    = Endpoint
        ConstructorName
        ConstructorArgs
        VarBindings
        HandlerSettings
        Mutability
        EpReturnType
    | SubApi ConstructorName ConstructorArgs ApiSpec
    deriving (Show, Generic)

data HandlerSettings = HandlerSettings
    { contentTypes :: Type
    , verb :: Type
    }
    deriving (Show, Generic, Eq)

newtype ConstructorName = ConstructorName Name deriving (Show, Generic, Eq)
newtype EpReturnType = EpReturnType Type deriving (Show, Generic, Eq)
newtype GadtName = GadtName Name deriving (Show, Generic, Eq)
newtype GadtType = GadtType Type deriving (Show, Generic, Eq)

newtype UrlSegment = UrlSegment String deriving (Show, Generic, Eq)
newtype ConstructorArgs = ConstructorArgs [(String, Type)] deriving (Show, Generic, Eq)
newtype Runner = Runner Type deriving (Show, Generic, Eq)

-- | Carries information regarding how the API looks at the place we're currently at.
data ServerInfo = ServerInfo
    { baseGadt :: GadtName
    -- ^ Use as a prefix of all types
    , currentGadt :: GadtName
    , parentConstructors :: [ConstructorName]
    -- ^ To create good names without conflict
    , prefixSegments :: [UrlSegment]
    -- ^ Used to give a good name to the request body
    , options :: ApiOptions
    -- ^ The current options
    }
    deriving (Show, Generic)

data ServerGenState = ServerGenState
    { info :: ServerInfo
    , usedParamNames :: Set String
    }
    deriving (Show, Generic)

newtype ServerGenM a = ServerGenM {unServerGenM :: StateT ServerGenState Q a}
    deriving newtype (Functor, Applicative, Monad, MonadState ServerGenState, MonadFail)

data Pmatch = Pmatch
    { paramPart :: Name
    , paramName :: String
    , paramType :: Type
    }
    deriving (Show, Generic)

data ConstructorMatch = ConstructorMatch
    { xParam :: Name
    -- ^ Of kind ParamPart
    , constructorName :: Name
    , parameters :: [Pmatch]
    , finalType :: FinalConstructorTypeMatch
    }
    deriving (Show, Generic)

data SubActionMatch = SubActionMatch
    { constructorName :: Name
    , parameters :: [Pmatch]
    , subActionName :: Name
    , subActionType :: Type
    }
    deriving (Show, Generic)

data SubActionTypeMatch = SubActionTypeMatch
    deriving (Show, Generic)

data FinalConstructorTypeMatch = FinalConstructorTypeMatch
    { requestType :: RequestTypeMatch
    , returnType :: Type
    }
    deriving (Show, Generic)

data RequestTypeMatch = RequestTypeMatch
    { accessType :: Type
    , contentTypes :: Type
    , verb :: Type
    }
    deriving (Show, Generic)
