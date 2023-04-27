{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Server.Servant.MapModel where

import Control.Arrow
import Control.Monad
import DomainDriven.Server.Servant.TaggedSumOfApis
import Generics.SOP qualified as SOP
import Generics.SOP.BasicFunctors
import Generics.SOP.NP hiding (Projection)
import Generics.SOP.NS
import Servant hiding (inject)
import Prelude

import DomainDriven.Server.Servant.Server
import GHC.Generics qualified as GHC
import Generics.SOP.GGP

class MapModelAndEvent serverFrom serverTo model model' event event' where
    mapModelAndEvent
        :: (model -> model')
        -> (event' -> event)
        -> serverFrom
        -> serverTo

instance {-# OVERLAPS #-} MapModelAndEvent server server model model' event event' where
    mapModelAndEvent _ _ = id

instance
    MapModelAndEvent
        (CbCmdServer model' event' m a)
        (CbCmdServer model event m a)
        model
        model'
        event
        event'
    where
    mapModelAndEvent proj inj (CbCmd server) =
        CbCmd $ \transact ->
            server
                ( \action -> transact $
                    \model -> ((. proj) *** map inj) <$> action (proj model)
                )

instance
    Functor m
    => MapModelAndEvent
        (CmdServer model' event' m a)
        (CmdServer model event m a)
        model
        model'
        event
        event'
    where
    mapModelAndEvent proj inj (Cmd server) =
        Cmd $ \model -> ((. proj) *** map inj) <$> server (proj model)

instance
    MapModelAndEvent
        (QueryServer model' m a)
        (QueryServer model m a)
        model
        model'
        event
        event'
    where
    mapModelAndEvent proj _ (Query server) = Query $ server . proj

instance
    MapModelAndEvent
        (CbQueryServer model' m a)
        (CbQueryServer model m a)
        model
        model'
        event
        event'
    where
    mapModelAndEvent proj _ (CbQuery server) = CbQuery $ \model -> server (proj <$> model)

instance
    MapModelAndEvent server' server model model' event event'
    => MapModelAndEvent (a -> server') (a -> server) model model' event event'
    where
    mapModelAndEvent proj inj server a = mapModelAndEvent proj inj (server a)

instance
    ( MapModelAndEvent server1' server1 model model' event event'
    , MapModelAndEvent server2' server2 model model' event event'
    )
    => MapModelAndEvent
        (server1' :<|> server2')
        (server1 :<|> server2)
        model
        model'
        event
        event'
    where
    mapModelAndEvent proj inj (server :<|> server2) =
        mapModelAndEvent proj inj server :<|> mapModelAndEvent proj inj server2

instance
    ( MapModelAndEvent t u model model' event event'
    , MapModelAndEvent (NP I ts) (NP I us) model model' event event'
    )
    => MapModelAndEvent (NP I (t ': ts)) (NP I (u ': us)) model model' event event'
    where
    mapModelAndEvent proj inj (I t :* ts) =
        I (mapModelAndEvent proj inj t) :* mapModelAndEvent proj inj ts

instance
    MapModelAndEvent (NP I fields) (NP I fields') model model' event event'
    => MapModelAndEvent (SOP I '[fields]) (SOP I '[fields']) model model' event event'
    where
    mapModelAndEvent proj inj (SOP x) = case x of
        Z fields -> SOP (Z $ mapModelAndEvent proj inj fields)
        S xs -> case xs of {}

instance
    ( GHC.Generic (RecordOfServers a)
    , GHC.Generic (RecordOfServers b)
    , GFrom (RecordOfServers a)
    , GTo (RecordOfServers b)
    , MapModelAndEvent
        (SOP I (GCode (RecordOfServers a)))
        (SOP I (GCode (RecordOfServers b)))
        model
        model'
        event
        event'
    )
    => MapModelAndEvent (RecordOfServers a) (RecordOfServers b) model model' event event'
    where
    mapModelAndEvent proj inj = gto . mapModelAndEvent proj inj . gfrom

class MapModel serverFrom serverTo model model' where
    mapModel :: (model -> model') -> serverFrom -> serverTo

instance {-# OVERLAPS #-} MapModel server server model model' where
    mapModel _ = id

instance
    MapModel
        (CbCmdServer model' event m a)
        (CbCmdServer model event m a)
        model
        model'
    where
    mapModel proj (CbCmd server) =
        CbCmd $ \transact ->
            server
                ( \action ->
                    transact $ \model -> first (. proj) <$> action (proj model)
                )

instance
    Functor m
    => MapModel
        (CmdServer model' event m a)
        (CmdServer model event m a)
        model
        model'
    where
    mapModel proj (Cmd server) =
        Cmd $ \model -> first (. proj) <$> server (proj model)

instance MapModel (QueryServer model' m a) (QueryServer model m a) model model' where
    mapModel proj (Query server) = Query $ server . proj

instance MapModel (CbQueryServer model' m a) (CbQueryServer model m a) model model' where
    mapModel proj (CbQuery server) = CbQuery $ \model -> server (proj <$> model)

instance
    MapModel server' server model model'
    => MapModel (a -> server') (a -> server) model model'
    where
    mapModel proj server a = mapModel proj (server a)

instance
    ( MapModel server1' server1 model model'
    , MapModel server2' server2 model model'
    )
    => MapModel (server1' :<|> server2') (server1 :<|> server2) model model'
    where
    mapModel proj (server :<|> server2) = mapModel proj server :<|> mapModel proj server2

instance
    (MapModel t u model model', MapModel (NP I ts) (NP I us) model model')
    => MapModel (NP I (t ': ts)) (NP I (u ': us)) model model'
    where
    mapModel proj (I t :* ts) = I (mapModel proj t) :* mapModel proj ts

instance
    MapModel (NP I fields) (NP I fields') model model'
    => MapModel (SOP I '[fields]) (SOP I '[fields']) model model'
    where
    mapModel proj (SOP x) = case x of
        Z fields -> SOP (Z $ mapModel proj fields)
        S xs -> case xs of {}

instance
    ( SOP.Generic (RecordOfServers a)
    , SOP.Generic (RecordOfServers b)
    , MapModel
        (SOP I (SOP.Code (RecordOfServers a)))
        (SOP I (SOP.Code (RecordOfServers b)))
        model
        model'
    )
    => MapModel (RecordOfServers a) (RecordOfServers b) model model'
    where
    mapModel proj = SOP.to . mapModel proj . SOP.from

class MapEvent serverFrom serverTo event event' where
    mapEvent :: (event' -> event) -> serverFrom -> serverTo

instance
    MapEvent
        (CbCmdServer model event' m a)
        (CbCmdServer model event m a)
        event
        event'
    where
    mapEvent inj (CbCmd server) =
        CbCmd $ \transact ->
            server (\action -> transact $ fmap (second (map inj)) . action)

instance
    Functor m
    => MapEvent
        (CmdServer model event' m a)
        (CmdServer model event m a)
        event
        event'
    where
    mapEvent inj (Cmd server) =
        Cmd $ fmap (second (map inj)) . server

instance MapEvent EmptyServer EmptyServer event event' where
    mapEvent _ server = server

instance
    MapEvent server' server event event'
    => MapEvent (a -> server') (a -> server) event event'
    where
    mapEvent inj server a = mapEvent inj (server a)

instance {-# OVERLAPS #-} MapEvent server server event event' where
    mapEvent _ = id

instance
    ( MapEvent server1' server1 event event'
    , MapEvent server2' server2 event event'
    )
    => MapEvent (server1' :<|> server2') (server1 :<|> server2) event event'
    where
    mapEvent inj (server :<|> server2) = mapEvent inj server :<|> mapEvent inj server2

instance MapEvent (NP I '[]) (NP I '[]) event event' where
    mapEvent _ Nil = Nil

instance
    (MapEvent t u model model', MapEvent (NP I ts) (NP I us) model model')
    => MapEvent (NP I (t ': ts)) (NP I (u ': us)) model model'
    where
    mapEvent inj (I t :* ts) = I (mapEvent inj t) :* mapEvent inj ts

instance
    MapEvent (NP I fields) (NP I fields') event event'
    => MapEvent (SOP I '[fields]) (SOP I '[fields']) event event'
    where
    mapEvent inj (SOP x) = case x of
        Z fields -> SOP (Z $ mapEvent inj fields)
        S xs -> case xs of {}

instance
    ( SOP.Generic (RecordOfServers a)
    , SOP.Generic (RecordOfServers b)
    , MapEvent
        (SOP I (SOP.Code (RecordOfServers a)))
        (SOP I (SOP.Code (RecordOfServers b)))
        event
        event'
    )
    => MapEvent (RecordOfServers a) (RecordOfServers b) event event'
    where
    mapEvent inj = SOP.to . mapEvent inj . SOP.from
