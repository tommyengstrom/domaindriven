module DomainDriven.Server.Param where

import Data.Kind
import Data.Proxy
import DomainDriven.Internal.Class
import GHC.TypeLits
import UnliftIO (MonadUnliftIO, liftIO)
import Prelude

------------------------------------------------------------------------------------------
-- Things to deal with the P type family
------------------------------------------------------------------------------------------
type PAction = ParamPart -> Type -> Type -> Type

-- | Used as a parameter to the `P` type family on order to determine the focus.
data ParamPart
    = ParamName
    | ParamType
    deriving (Show)

-- | P is used for specifying the parameters of the model.
-- The name will be used as the name in the JSON encoding or the query parameter of the
-- generated server.
type family P (x :: ParamPart) (name :: Symbol) (a :: Type) where
    P 'ParamName name ty = Proxy name
    P 'ParamType name ty = ty

runPAction
    :: (MonadUnliftIO m, WriteModel p, model ~ Model p, event ~ Event p)
    => p
    -> PActionHandler model event m cmd
    -> cmd 'ParamType method ret
    -> m ret
runPAction p handleCmd cmd = case handleCmd cmd of
    Query m -> m =<< liftIO (getModel p)
    CbQuery m -> m (liftIO (getModel p))
    Cmd m -> transactionalUpdate p m
    CbCmd withTrans -> withTrans $ \runTrans -> do
        transactionalUpdate p runTrans

type PActionHandler model event m c =
    forall method a. c 'ParamType method a -> HandlerType method model event m a

type PActionRunner m c =
    forall method a
     . MonadUnliftIO m
    => c 'ParamType method a
    -> m a

------------------------------------------------------------------------------------------
-- Done with the P type family things
------------------------------------------------------------------------------------------
--
--
--
--
