-- These instances can no doubt be simplified a little bit...
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Server.MapModel where

import Control.Arrow
import Control.Monad
import Data.Kind
import DomainDriven.Server.DomainDrivenApi
import DomainDriven.Server.Server
import GHC.Generics qualified as GHC
import GHC.TypeLits
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
    ( serverFrom ~ DomainDrivenServer mkServer modelTo index eventFrom m
    , MapModelAndEvent
        (DomainDrivenServer mkServer modelTo index eventFrom m)
        (DomainDrivenServer mkServer modelTo index eventTo m)
    )
    => MapEvent
        serverFrom
        (DomainDrivenServer mkServer modelTo index eventTo m)
    where
    mapEvent = mapModelAndEvent id

class MapModel serverFrom serverTo where
    mapModel
        :: (ModelTo serverTo -> ModelFrom serverFrom)
        -> serverFrom
        -> serverTo

instance
    ( serverFrom ~ DomainDrivenServer mkServer modelFrom index eventTo m
    , MapModelAndEvent
        (DomainDrivenServer mkServer modelFrom index eventTo m)
        (DomainDrivenServer mkServer modelTo index eventTo m)
    )
    => MapModel
        serverFrom
        (DomainDrivenServer mkServer modelTo index eventTo m)
    where
    mapModel f = mapModelAndEvent f id

instance
    ( mkServerFrom ~ mkServerTo
    , mFrom ~ mTo
    , MapModelAndEvent'
        modelFrom
        modelTo
        eventFrom
        eventTo
        (DomainDrivenServer mkServerTo modelFrom index eventFrom mTo)
        (DomainDrivenServer mkServerTo modelTo index eventTo mTo)
    )
    => MapModelAndEvent
        (DomainDrivenServer mkServerFrom modelFrom index eventFrom mFrom)
        (DomainDrivenServer mkServerTo modelTo index eventTo mTo)
    where
    type
        ModelFrom (DomainDrivenServer mkServerFrom modelFrom index eventFrom mFrom) =
            modelFrom
    type ModelTo (DomainDrivenServer mkServerTo modelTo index eventTo mTo) = modelTo
    type
        EventFrom (DomainDrivenServer mkServerFrom modelFrom index eventFrom mFrom) =
            eventFrom
    type EventTo (DomainDrivenServer mkServerTo modelTo index eventTo mTo) = eventTo
    mapModelAndEvent = mapModelAndEvent'

class MapModelAndEvent' modelFrom modelTo eventFrom eventTo serverFrom serverTo where
    mapModelAndEvent'
        :: (modelTo -> modelFrom)
        -> (eventFrom -> eventTo)
        -> serverFrom
        -> serverTo

-- FIXME: Do we more instances here now that there is a specific instance for `NP I '[]`?
instance
    {-# OVERLAPPABLE #-}
    ( serverFrom ~ serverTo
    , TypeError
        ( Text "Missing mapModelAndEvent' instance for: "
            :$$: ShowType serverFrom
            :$$: Text " to: "
            :$$: ShowType serverTo
        )
    )
    => MapModelAndEvent' modelFrom modelTo eventFrom eventTo serverFrom serverTo
    where
    mapModelAndEvent' _ _ = id

instance MapModelAndEvent' modelFrom modelTo eventFrom eventTo (NP I '[]) (NP I '[]) where
    mapModelAndEvent' _ _ = id

------------------------------
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
        (CmdServer modelFrom' eventFrom' mFrom aFrom)
        (CmdServer modelTo' eventTo' mTo aTo)
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
        (QueryServer modelFrom' mFrom aFrom)
        (QueryServer modelTo' mTo aTo)
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
        (CbQueryServer modelFrom' mFrom aFrom)
        (CbQueryServer modelTo' mTo aTo)
    where
    mapModelAndEvent' proj _ (CbQuery server) = CbQuery $ \model -> server (proj <$> model)

------------------------------------
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
        (CbCmdServerI modelFrom' index eventFrom' mFrom aFrom)
        (CbCmdServerI modelTo' index eventTo' mTo aTo)
    where
    mapModelAndEvent' proj inj (CbCmdI index server) =
        CbCmdI index $ \transact ->
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
        (CmdServerI modelFrom' index eventFrom' mFrom aFrom)
        (CmdServerI modelTo' index eventTo' mTo aTo)
    where
    mapModelAndEvent' proj inj (CmdI index server) =
        CmdI index $ \model -> ((. proj) *** map inj) <$> server (proj model)

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
        (QueryServerI modelFrom' index mFrom aFrom)
        (QueryServerI modelTo' index mTo aTo)
    where
    mapModelAndEvent' proj _ (QueryI index server) = QueryI index $ server . proj

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
        (CbQueryServerI modelFrom' index mFrom aFrom)
        (CbQueryServerI modelTo' index mTo aTo)
    where
    mapModelAndEvent' proj _ (CbQueryI index server) = CbQueryI index $ \model -> server (proj <$> model)

instance
    (aFrom ~ aTo, MapModelAndEvent' modelFrom modelTo eventFrom eventTo serverFrom serverTo)
    => MapModelAndEvent'
        modelFrom
        modelTo
        eventFrom
        eventTo
        (aFrom -> serverFrom)
        (aTo -> serverTo)
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
    ( GHC.Generic (DomainDrivenServer mkServer modelFrom index eventFrom m)
    , GHC.Generic (DomainDrivenServer mkServer modelTo index eventTo m)
    , GFrom (DomainDrivenServer mkServer modelFrom index eventFrom m)
    , GTo (DomainDrivenServer mkServer modelTo index eventTo m)
    , MapModelAndEvent'
        modelFrom
        modelTo
        eventFrom
        eventTo
        (SOP I (GCode (DomainDrivenServer mkServer modelFrom index eventFrom m)))
        (SOP I (GCode (DomainDrivenServer mkServer modelTo index eventTo m)))
    )
    => MapModelAndEvent'
        modelFrom
        modelTo
        eventFrom
        eventTo
        (DomainDrivenServer mkServer modelFrom index eventFrom m)
        (DomainDrivenServer mkServer modelTo index eventTo m)
    where
    mapModelAndEvent' proj inj = gto . mapModelAndEvent' proj inj . gfrom
