{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Server.MapModel where

import Control.Arrow
import Control.Monad
import Data.Kind
import DomainDriven.Server.DomainDrivenApi
import DomainDriven.Server.Server
import GHC.Generics qualified as GHC
import Generics.SOP.BasicFunctors
import Generics.SOP.GGP
import Generics.SOP.NP hiding (Projection)
import Generics.SOP.NS
import Servant hiding (inject)
import Prelude

class MapModelAndEvent serverFrom serverTo where
    type ModelFrom serverFrom :: Type
    type ModelTo serverTo :: Type
    type EventFrom serverFrom :: Type
    type EventTo serverTo :: Type
    mapModelAndEvent
        :: (ModelTo serverTo -> ModelFrom serverFrom)
        -> (EventFrom serverFrom -> EventTo serverTo)
        -> serverFrom
        -> serverTo

class MapEvent serverFrom serverTo where
    mapEvent
        :: (EventFrom serverFrom -> EventTo serverTo)
        -> serverFrom
        -> serverTo

instance
    ( MapModelAndEvent serverFrom serverTo
    , ModelFrom serverFrom ~ ModelTo serverTo
    )
    => MapEvent serverFrom serverTo
    where
    mapEvent = mapModelAndEvent id

class MapModel serverFrom serverTo where
    mapModel
        :: (ModelTo serverTo -> ModelFrom serverFrom)
        -> serverFrom
        -> serverTo

instance
    ( MapModelAndEvent serverFrom serverTo
    , EventFrom serverFrom ~ EventTo serverTo
    )
    => MapModel serverFrom serverTo
    where
    mapModel f = mapModelAndEvent f id

instance
    MapModelAndEvent'
        (ModelFrom (DomainDrivenServer modelFrom eventFrom recordFrom))
        (ModelTo (DomainDrivenServer modelTo eventTo recordTo))
        (EventFrom (DomainDrivenServer modelFrom eventFrom recordFrom))
        (EventTo (DomainDrivenServer modelTo eventTo recordTo))
        (DomainDrivenServer modelFrom eventFrom recordFrom)
        (DomainDrivenServer modelTo eventTo recordTo)
    => MapModelAndEvent
        (DomainDrivenServer modelFrom eventFrom recordFrom)
        (DomainDrivenServer modelTo eventTo recordTo)
    where
    type ModelFrom (DomainDrivenServer modelFrom eventFrom recordFrom) = modelFrom
    type ModelTo (DomainDrivenServer modelTo eventTo recordTo) = modelTo
    type EventFrom (DomainDrivenServer modelFrom eventFrom recordFrom) = eventFrom
    type EventTo (DomainDrivenServer modelTo eventTo recordTo) = eventTo
    mapModelAndEvent = mapModelAndEvent'

class MapModelAndEvent' modelFrom modelTo eventFrom eventTo serverFrom serverTo where
    mapModelAndEvent'
        :: (modelTo -> modelFrom)
        -> (eventFrom -> eventTo)
        -> serverFrom
        -> serverTo

instance
    {-# OVERLAPPABLE #-}
    (serverFrom ~ serverTo)
    => MapModelAndEvent' modelFrom modelTo eventFrom eventTo serverFrom serverTo
    where
    mapModelAndEvent' _ _ = id

instance
    ( modelFrom ~ modelFrom'
    , eventFrom ~ eventFrom'
    , modelTo ~ modelTo'
    , eventTo ~ eventTo'
    , aFrom ~ aTo
    , mFrom ~ mTo
    )
    => MapModelAndEvent'
        modelFrom
        modelTo
        eventFrom
        eventTo
        (CbCmdServer modelFrom' eventFrom' mFrom aFrom)
        (CbCmdServer modelTo' eventTo' mTo aTo)
    where
    mapModelAndEvent' proj inj (CbCmd server) =
        CbCmd $ \transact ->
            server
                ( \action -> transact $
                    \model -> ((. proj) *** map inj) <$> action (proj model)
                )

instance
    ( modelFrom ~ modelFrom'
    , eventFrom ~ eventFrom'
    , modelTo ~ modelTo'
    , eventTo ~ eventTo'
    , aFrom ~ aTo
    , mFrom ~ mTo
    , Functor mFrom
    )
    => MapModelAndEvent'
        modelFrom
        modelTo
        eventFrom
        eventTo
        (CmdServer modelFrom eventFrom mFrom aFrom)
        (CmdServer modelTo eventTo mTo aTo)
    where
    mapModelAndEvent' proj inj (Cmd server) =
        Cmd $ \model -> ((. proj) *** map inj) <$> server (proj model)

instance
    ( modelFrom ~ modelFrom'
    , modelTo ~ modelTo'
    , aFrom ~ aTo
    , mFrom ~ mTo
    )
    => MapModelAndEvent'
        modelFrom
        modelTo
        eventFrom
        eventTo
        (QueryServer modelFrom mFrom aFrom)
        (QueryServer modelTo mTo aTo)
    where
    mapModelAndEvent' proj _ (Query server) = Query $ server . proj

instance
    ( modelFrom ~ modelFrom'
    , modelTo ~ modelTo'
    , aFrom ~ aTo
    , mFrom ~ mTo
    )
    => MapModelAndEvent'
        modelFrom
        modelTo
        eventFrom
        eventTo
        (CbQueryServer modelFrom mFrom aFrom)
        (CbQueryServer modelTo mTo aTo)
    where
    mapModelAndEvent' proj _ (CbQuery server) = CbQuery $ \model -> server (proj <$> model)

instance
    (aFrom ~ aTo, MapModelAndEvent' modelFrom modelTo eventFrom eventTo serverFrom serverTo)
    => MapModelAndEvent' modelFrom modelTo eventFrom eventTo (aFrom -> serverFrom) (aTo -> serverTo)
    where
    mapModelAndEvent' proj inj server a = mapModelAndEvent' proj inj (server a)

instance
    ( MapModelAndEvent' modelFrom modelTo eventFrom eventTo serverFrom1 serverTo1
    , MapModelAndEvent' modelFrom modelTo eventFrom eventTo serverFrom2 serverTo2
    )
    => MapModelAndEvent'
        modelFrom
        modelTo
        eventFrom
        eventTo
        (serverFrom1 :<|> serverFrom2)
        (serverTo1 :<|> serverTo2)
    where
    mapModelAndEvent' proj inj (server :<|> server2) =
        mapModelAndEvent' proj inj server :<|> mapModelAndEvent' proj inj server2

instance
    ( MapModelAndEvent' modelFrom modelTo eventFrom eventTo t u
    , MapModelAndEvent' modelFrom modelTo eventFrom eventTo (NP I ts) (NP I us)
    )
    => MapModelAndEvent' modelFrom modelTo eventFrom eventTo (NP I (t ': ts)) (NP I (u ': us))
    where
    mapModelAndEvent' proj inj (I t :* ts) =
        I (mapModelAndEvent' proj inj t) :* mapModelAndEvent' proj inj ts

instance
    MapModelAndEvent' modelFrom modelTo eventFrom eventTo (NP I fields) (NP I fields')
    => MapModelAndEvent' modelFrom modelTo eventFrom eventTo (SOP I '[fields]) (SOP I '[fields'])
    where
    mapModelAndEvent' proj inj (SOP x) = case x of
        Z fields -> SOP (Z $ mapModelAndEvent' proj inj fields)
        S xs -> case xs of {}

instance
    ( GHC.Generic (DomainDrivenServer modelFrom eventFrom recordFrom)
    , GHC.Generic (DomainDrivenServer modelTo eventTo recordTo)
    , GFrom (DomainDrivenServer modelFrom eventFrom recordFrom)
    , GTo (DomainDrivenServer modelTo eventTo recordTo)
    , MapModelAndEvent'
        modelFrom
        modelTo
        eventFrom
        eventTo
        (SOP I (GCode (DomainDrivenServer modelFrom eventFrom recordFrom)))
        (SOP I (GCode (DomainDrivenServer modelTo eventTo recordTo)))
    )
    => MapModelAndEvent'
        modelFrom
        modelTo
        eventFrom
        eventTo
        (DomainDrivenServer modelFrom eventFrom recordFrom)
        (DomainDrivenServer modelTo eventTo recordTo)
    where
    mapModelAndEvent' proj inj = gto . mapModelAndEvent' proj inj . gfrom
