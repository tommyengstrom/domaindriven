module DomainDriven.Server.Types where

import qualified Data.Map                                     as M
import           Data.Text                      ( Text )
import           DomainDriven.Internal.Class
import           GHC.Generics                   ( Generic )
import           Language.Haskell.TH
import           Prelude

-- Contains infromatiotion of how the API should look, gathered from the Action GADT.
data ApiSpec = ApiSpec
    { gadtName   :: GadtName -- ^ Name of the GADT representing the command
    , endpoints  :: [ApiPiece] -- ^ Endpoints created from the constructors of the GADT
    , apiOptions :: ApiOptions -- ^ The setting to use when generating part of the API
    }
    deriving (Show, Generic)

data ActionType
    = Mutable
    | Immutable
    deriving (Show, Eq)

-- data ActionRunner = ActionRunner Type
--     deriving (Show, Eq)

data ApiPiece
    = Endpoint ConstructorName ConstructorArgs HandlerSettings ActionType EpReturnType
    | SubApi ConstructorName ConstructorArgs ApiSpec
    deriving (Show, Generic)

data HandlerSettings = HandlerSettings
    { contentTypes :: Type
    , verb         :: Type
    }
    deriving (Show, Generic, Eq)

newtype ConstructorName = ConstructorName Name          deriving (Show, Generic, Eq)
newtype EpReturnType    = EpReturnType Type             deriving (Show, Generic, Eq)
newtype GadtName        = GadtName Name                 deriving (Show, Generic, Eq)
newtype GadtType        = GadtType {unGadtType :: Type} deriving (Show, Generic, Eq)
newtype UrlSegment      = UrlSegment String             deriving (Show, Generic, Eq)
newtype ConstructorArgs = ConstructorArgs [Type]        deriving (Show, Generic, Eq)
newtype Runner          = Runner Type                   deriving (Show, Generic, Eq)

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
