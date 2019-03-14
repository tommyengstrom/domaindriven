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

data Command i r = Command
    { getCommand :: forall m . EventSourced m => i
                 -> ReaderT (StmState m) IO (Event m, r)}

type family CmdRunner m a :: Type where
    CmdRunner m (Command i r) = i -> ReaderT (StmState m) IO (Event m, r)
    CmdRunner m (a :|| b)   = CmdRunner m a :|| CmdRunner m b

class HasCmdHandler i r cmds where
    getCmdHandler :: cmds -> Command i r

instance HasCmdHandler i r (Command i r) where
    getCmdHandler = id

instance {-# Overlapping #-} HasCmdHandler i r (Command i r :|| b) where
    getCmdHandler (a :|| _) = a

instance HasCmdHandler i r b => HasCmdHandler i r (a :|| b) where
    getCmdHandler (_ :|| b) = getCmdHandler b

getHandler :: HasCmdHandler i r cmds => cmds -> Proxy (Command i r) -> Command i r
getHandler cmds _ = getCmdHandler cmds

class EventSourced a where
    type Event a :: Type
    type Cmds a :: Type  -- Something built using `:||` and `Command i r`

    applyEvent :: Stored (Event a) -> ReaderT (StmState a) IO ()
    cmdHandlers :: CmdRunner a (Cmds a)

    runCmd :: forall i r. HasCmdHandler i r (CmdRunner a (Cmds a))
           => i -> ReaderT (StmState a) IO r
    runCmd i = do
        (ev, r) <- getCommand (getHandler (cmdHandlers @a) (Proxy @(Command i r))) i
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


-- I think this approach may actually work out okay. Though I'm not quite sure about
-- how I could generate a server from this. I guess I need template haskell anyway.
--
-- The upside is that now each command can be a record and I have the structures already,
-- so the only thing I need template haskell for is to fetch the class instances (I think)
class HasCmd model cmd where
    type Event' cmd :: Type
    type Return' cmd :: Type
    applyEvent' :: Stored (Event' cmd) -> ReaderT (StmState model) IO ()
    evalCmd' :: cmd -> ReaderT (StmState model) IO (Event' cmd, Return' cmd)

