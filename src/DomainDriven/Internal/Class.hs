{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module DomainDriven.Internal.Class where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Kind
import           Data.UUID
import           RIO
import           RIO.Time
import           System.Random

data StmState x = StmState
    { writeEvent :: Stored (Event x) -> IO ()
    , readEvents :: IO [Event x]
    , currentState :: TVar x
    }

data a :|| b = a :|| b
    deriving (Show)
infixr 8 :||

data Command cmd = Command
    { getCommand :: forall m . (EventSourced m) => cmd
                 -> ReaderT (StmState m) IO (Event m, Returns cmd)}

type family Returns a :: Type

-- class EventSourced model => Cmd cmd model | cmd -> model where
--     type Return cmd :: Type
--     cmdRunner :: cmd -> ReaderT (StmState model) IO (Event model, Return cmd)

type family CmdRunner m a :: Type where
    CmdRunner m (Command i) = i -> ReaderT (StmState m) IO (Event m, Returns i)
    CmdRunner m (a :|| b)   = CmdRunner m a :|| CmdRunner m b

class EventSourced a where
    type Event a :: Type
    type Cmds a :: Type  -- Something built using `:||` and `Command i r`

    applyEvent :: Stored (Event a) -> ReaderT (StmState a) IO ()
    cmdHandlers :: CmdRunner a (Cmds a)

    runCmd :: forall i. PickType (Command i) (CmdRunner a (Cmds a))
           => i -> ReaderT (StmState a) IO (Returns i)
    runCmd i = do
        -- (ev, r) <- getCommand (pickType (cmdHandlers @a) :: i -> ReaderT (StmState a) IO (Event a, Returns i)) i
        (ev, r) <- getCommand (pickType (cmdHandlers @a) :: Command i) i
        s <- toStored ev
        applyEvent s
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
toStored :: MonadIO m => e -> m (Stored e)
toStored e = Stored e <$> getCurrentTime <*> mkId id


class PickType t ts where
    pickType :: ts -> t

instance PickType t t where
    pickType = id

instance {-# Overlapping #-} PickType t (t :|| ts) where
    pickType (a :|| _) = a

instance PickType t ts => PickType t (a :|| ts) where
    pickType (_ :|| ts) = pickType ts

