{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DomainDriven.Server.Servant where

import Control.Monad
import Control.Monad.Catch qualified
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.Types
import Data.Kind
import DomainDriven.Persistance.Class
import GHC.TypeLits
import Generics.SOP qualified as SOP
import Generics.SOP.BasicFunctors
import Generics.SOP.NP hiding (Projection)
import Generics.SOP.NS
import Servant hiding (inject)
import Servant.Server.Generic
import Servant.Server.Internal.Delayed
import UnliftIO hiding (Handler)
import Prelude

data Cmd (model :: Type) (event :: Type) (verb :: Type)

data Query (model :: Type) (verb :: Type)

data CbQuery (model :: Type) (verb :: Type)

data CbCmd (model :: Type) (event :: Type) (verb :: Type)

type family CanMutate (method :: StdMethod) :: Bool where
  CanMutate 'GET = 'False
  CanMutate 'POST = 'True
  CanMutate 'PUT = 'True
  CanMutate 'PATCH = 'True
  CanMutate 'DELETE = 'True
  CanMutate method = TypeError ('Text "CanMutate is not defined for " ':<>: 'ShowType method)

mapServer :: (a -> b) -> Delayed env a -> Delayed env b
mapServer f Delayed {..} =
  Delayed
    { serverD = \c p h a b req -> fmap f (serverD c p h a b req),
      ..
    }

-- should use proxies?
-- no! wrap types instead. less magic and avoids type annotations
-- type ClaimTypeApi lc model events = TaggedSumOfApis (MkClaimTypeApi lc model events)
-- need to transform through TaggedSumOfApis...
gmapModel ::
  forall m mkModelApi model model' event event'.
  ( SOP.Generic (mkModelApi model' event' (AsServerT m)),
    SOP.Generic (mkModelApi model event (AsServerT m)),
    GMapModel
      m
      model
      model'
      event
      event'
      (SOP.Code (mkModelApi model' event' AsApi))
      (SOP.Code (mkModelApi model event AsApi))
      (SOP.Code (mkModelApi model' event' (AsServerT m)))
      (SOP.Code (mkModelApi model event (AsServerT m)))
  ) =>
  (model -> model') ->
  (event' -> event) ->
  mkModelApi model' event' (AsServerT m) ->
  mkModelApi model event (AsServerT m)
gmapModel proj inj = SOP.to . SOP . gmapModel'' @m @model @model' @event @event' @(SOP.Code (mkModelApi model' event' AsApi)) @(SOP.Code (mkModelApi model event AsApi)) @_ @_ proj inj . unSOP . SOP.from

{- gmapModel ::
  forall mkApiFrom mkApiTo m model model' event event' .
  (SOP.Generic (mkApiFrom (AsServerT m)), SOP.Generic (mkApiTo (AsServerT m))
  , GMapModel m model model' event event' (SOP.Code (mkApiFrom AsApi)) (SOP.Code (mkApiTo AsApi)) (SOP.Code (mkApiFrom (AsServerT m))) (SOP.Code (mkApiTo (AsServerT m)))
  ) =>
  (model -> model') ->
  (event' -> event) ->
  mkApiFrom (AsServerT m) ->
  mkApiTo (AsServerT m)
gmapModel proj inj = SOP.to . SOP . gmapModel'' @m @model @model' @event @event' @(SOP.Code (mkApiFrom AsApi)) @(SOP.Code (mkApiTo AsApi)) @_ @_ proj inj . unSOP . SOP.from
 -}
class GMapModel m model model' event event' (apisFrom :: [[Type]]) (apisTo :: [[Type]]) (serversFrom :: [[Type]]) (serversTo :: [[Type]]) where
  gmapModel'' :: (model -> model') -> (event' -> event) -> NS (NP I) serversFrom -> NS (NP I) serversTo

instance
  GMapModel' m model model' event event' apisFrom apisTo serversFrom serversTo =>
  GMapModel m model model' event event' '[apisFrom] '[apisTo] '[serversFrom] '[serversTo]
  where
  gmapModel'' proj inj = \case
    Z servers -> Z (gmapModel' @m @model @model' @event @event' @apisFrom @apisTo proj inj servers)
    S x -> case x of {}

class GMapModel' m model model' event event' (apisFrom :: [Type]) (apisTo :: [Type]) (serversFrom :: [Type]) (serversTo :: [Type]) where
  gmapModel' :: (model -> model') -> (event' -> event) -> NP I serversFrom -> NP I serversTo

instance GMapModel' m model model' event event' '[] '[] '[] '[] where
  gmapModel' _ _ Nil = Nil

instance
  ( serverFrom ~ ServerT apiFrom m,
    serverTo ~ ServerT apiTo m,
    MapModel m model model' event event' apiFrom apiTo,
    GMapModel' m model model' event event' apisFrom apisTo serversFrom serversTo
  ) =>
  GMapModel' m model model' event event' (apiFrom ': apisFrom) (apiTo ': apisTo) (serverFrom ': serversFrom) (serverTo ': serversTo)
  where
  gmapModel' proj inj (I server :* servers) =
    I (mapModel @m @model @model' @event @event' @apiFrom @apiTo proj inj server)
      :* gmapModel' @m @model @model' @event @event' @apisFrom @apisTo proj inj servers

class MapModel m model model' event event' apiFrom apiTo where
  mapModel ::
    (model -> model') ->
    (event' -> event) ->
    ServerT apiFrom m ->
    ServerT apiTo m

instance MapModel m model model' event event' (CbCmd model' event' (Verb (method :: StdMethod) status ctypes a)) (CbCmd model event (Verb method status ctypes a)) where
  mapModel proj inj h (TransactionalUpdate transact) =
    h
      ( TransactionalUpdate $ \action ->
          transact $
            \model -> (\(mkRes, events) -> (mkRes . proj, map inj events)) <$> action (proj model)
      )

instance Functor m => MapModel m model model' event event' (Cmd model' event' (Verb (method :: StdMethod) status ctypes a)) (Cmd model event (Verb method status ctypes a)) where
  mapModel proj inj h model = (\(mkRes, events) -> (mkRes . proj, map inj events)) <$> h (proj model)

instance MapModel m model model' event event' (Query model' (Verb (method :: StdMethod) status ctypes a)) (Query model (Verb method status ctypes a)) where
  mapModel proj inj h model = h (proj model)

instance MapModel m model model' event event' (CbQuery model' (Verb (method :: StdMethod) status ctypes a)) (CbQuery model (Verb method status ctypes a)) where
  mapModel proj inj h getModel = h (proj <$> getModel)

instance MapModel m model model' event event' EmptyAPI EmptyAPI where
  mapModel _ _ = id

instance MapModel m model model' event event' api' api => MapModel m model model' event event' (QueryParam' mods s a :> api') (QueryParam' mods s a :> api) where
  mapModel proj inj server p = mapModel @m @model @model' @event @event' @api' @api proj inj (server p)

instance MapModel m model model' event event' api' api => MapModel m model model' event event' (QueryFlag s :> api') (QueryFlag s :> api) where
  mapModel proj inj server f = mapModel @m @model @model' @event @event' @api' @api proj inj (server f)

instance MapModel m model model' event event' api' api => MapModel m model model' event event' (ReqBody contentType a :> api') (ReqBody contentType a :> api) where
  mapModel proj inj server b = mapModel @m @model @model' @event @event' @api' @api proj inj (server b)

instance MapModel m model model' event event' api' api => MapModel m model model' event event' ((path :: Symbol) :> api') ((path :: Symbol) :> api) where
  mapModel = mapModel @m @model @model' @event @event' @api' @api

instance MapModel m model model' event event' api' api => MapModel m model model' event event' (Description desc :> api') (Description desc :> api) where
  mapModel = mapModel @m @model @model' @event @event' @api' @api

instance MapModel m model model' event event' api' api => MapModel m model model' event event' (Capture' mods capture a :> api') (Capture' mods capture a :> api) where
  mapModel proj inj server c = mapModel @m @model @model' @event @event' @api' @api proj inj (server c)

-- I won't bother with read-only models in the server context...
data WriteModel' model events where
  WriteModel' :: WriteModel p => p -> WriteModel' (Model p) (Event p)

instance
  ( HasContextEntry context (WriteModel' model event),
    HasServer (Verb method status ctypes a) context,
    CanMutate method ~ 'True
  ) =>
  HasServer (Cmd model event (Verb method status ctypes a)) context
  where
  type ServerT (Cmd model event (Verb method status ctypes a)) m = model -> m (model -> a, [event])
  hoistServerWithContext _ _ f action model = f (action model)

  route _ context delayedServer = case getContextEntry context :: WriteModel' model event of
    WriteModel' p ->
      route (Proxy @(Verb method status ctypes a)) context $
        mapServer
          ( \server -> do
              handlerRes <-
                liftIO . Control.Monad.Catch.try . transactionalUpdate p $
                  either throwIO pure <=< runHandler . server
              either throwError pure handlerRes
          )
          delayedServer

instance
  ( HasContextEntry context (WriteModel' model event),
    HasServer (Verb method status ctypes a) context
  ) =>
  HasServer (Query model (Verb method status ctypes a)) context
  where
  type ServerT (Query model (Verb method status ctypes a)) m = model -> m a

  hoistServerWithContext _ _ f action model = f (action model)

  route _ context delayedServer = case getContextEntry context :: WriteModel' model event of
    WriteModel' p ->
      route (Proxy @(Verb method status ctypes a)) context $
        mapServer (=<< liftIO (getModel p)) delayedServer

instance
  ( HasContextEntry context (WriteModel' model event),
    HasServer (Verb method status ctypes a) context
  ) =>
  HasServer (CbQuery model (Verb method status ctypes a)) context
  where
  type ServerT (CbQuery model (Verb method status ctypes a)) m = IO model -> m a

  hoistServerWithContext _ _ f action model = f (action model)

  route _ context delayedServer = case getContextEntry context :: WriteModel' model event of
    WriteModel' p ->
      route (Proxy @(Verb method status ctypes a)) context $
        mapServer ($ getModel p) delayedServer

-- We have to wrap the transact function to avoid  'Illegal polymorphic or qualified type in typeclass'
newtype TransactionalUpdate model event
  = TransactionalUpdate (forall m x. MonadUnliftIO m => (model -> m (model -> x, [event])) -> m x)

instance
  ( HasContextEntry context (WriteModel' model event),
    HasServer (Verb method status ctypes a) context,
    CanMutate method ~ 'True
  ) =>
  HasServer (CbCmd model event (Verb method status ctypes a)) context
  where
  type ServerT (CbCmd model event (Verb method status ctypes a)) m = TransactionalUpdate model event -> m a

  hoistServerWithContext _ _ f action model = f (action model)

  route _ context delayedServer = case getContextEntry context :: WriteModel' model event of
    WriteModel' p ->
      route (Proxy @(Verb method status ctypes a)) context $
        mapServer ($ TransactionalUpdate (transactionalUpdate p)) delayedServer

data NamedField = NamedField Symbol Type

type family FieldTypes (fields :: [NamedField]) :: [Type] where
  FieldTypes '[] = '[]
  FieldTypes ('NamedField _ t ': fields) = t ': FieldTypes fields

newtype Field a = Field a

newtype JsonObject (fields :: [NamedField]) = JsonObject (NP Field (FieldTypes fields))

type TestApi = ReqBody '[JSON] (JsonObject '[ 'NamedField "asd" Int, 'NamedField "bsd" Int])

class ParseFields (fields :: [NamedField]) where
  parseFields :: Object -> Parser (NP Field (FieldTypes fields))

instance ParseFields '[] where
  parseFields _ = pure Nil

instance (ParseFields fields, FromJSON t, KnownSymbol name) => ParseFields ('NamedField name t ': fields) where
  parseFields o = do
    fields <- parseFields @fields o
    t <- o .: fromString (symbolVal (Proxy @name))
    pure $ Field t :* fields

instance ParseFields fields => FromJSON (JsonObject fields) where
  parseJSON = withObject "JsonObject" $ \o -> JsonObject <$> parseFields @fields o
