{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
module DomainDriven.Internal.Class where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Kind
import           Data.UUID
import           RIO
import           RIO.Time
import           System.Random

class Monad m => ESRunner m where
    type Event m :: Type
    type Model m :: Type
    type Cmd m :: Type -> Type
    -- readEvents :: m [Stored (Event m)]
    persistEvent :: Event m -> m (Stored (Event m))
    applyEvent :: Stored (Event m) -> m ()
    evalCmd :: Cmd m a -> m (Event m, a)

    runCmd :: Cmd m a -> m a
    runCmd cmd = do
        (ev, r) <- evalCmd cmd
        applyEvent =<< persistEvent ev
        pure r

data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

mkId :: MonadIO m =>  (UUID -> b) -> m b
mkId c = c <$> liftIO randomIO

-- Without a functional dep on EvenSourced (m -> model) model will have to be specified
-- when running.
toStored :: forall m. (MonadIO m, ESRunner m)
         => Event m -> m (Stored (Event m))
toStored e = Stored e <$> getCurrentTime <*> mkId id


-- type family Ret (a :: k) :: Type
--
-- class Cqrs m cmd where
--     cqrs :: Monad m => cmd -> m (Ret cmd)
--     cqrs = undefined
--
-- type instance Ret 'MyDeleteHead = ()
-- type instance Ret ('MyAppend _) = Int
--
-- data MyCmd
--     = MyAppend Int
--     | MyDeleteHead
--
-- instance Cqrs IO MyCmd where
--     cqrs (MyAppend i) = pure i
--     cqrs MyDeleteHead = pure ()
--------------------
--
-- data Something
--     = Bare
--     | WithString String
--
-- type family F (a :: k) :: Type
-- type instance F 'Bare = ()
-- type instance F ('WithString t) = Int
--
