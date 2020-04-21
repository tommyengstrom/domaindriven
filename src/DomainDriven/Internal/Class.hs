module DomainDriven.Internal.Class where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.UUID
import           RIO
import           RIO.Time
import           System.Random
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import qualified RIO.ByteString.Lazy                          as BL
import qualified RIO.ByteString                               as BS
import           Data.Char                      ( ord )
import           System.Directory               ( doesFileExist )
import           Control.Monad.Loops            ( whileM_ )

data Domain model event = Domain
    { persistance :: Persistance event
    , applyEvent :: model -> Stored event -> model
    , model :: TVar model
    }

type CmdHandler model event cmd err
    = forall a . Exception err => cmd a -> IO (model -> Either err (a, [event]))

type CmdRunner c = forall a . c a -> IO a
type QueryRunner c = forall a . c a -> IO a

runCmd
    :: Exception err
    => Domain model event
    -> CmdHandler model event cmd err
    -> cmd a
    -> IO a
runCmd (Domain pm appEvent tvar) cmdRunner cmd = do
    cmdTransaction <- cmdRunner cmd
    atomically $ do
        m         <- readTVar tvar
        (r, evs)  <- either throwM pure $ cmdTransaction m
        storedEvs <- traverse (persistEvent pm) evs
        let newModel = foldl' appEvent m storedEvs
        writeTVar tvar newModel
        pure r

createModel
    :: Persistance event
    -> (model -> Stored event -> model)
    -> model -- ^ initial model
    -> IO (Domain model event)
createModel p@(Persistance chan) apply m0 = do
    tvar <- newTVarIO m0
    whileM_ (atomically . fmap not $ isEmptyTChan chan) . atomically $ do
        m <- readTVar tvar
        e <- readTChan chan
        writeTVar tvar $ apply m e
    pure $ Domain p apply tvar

-- This should really contain  `TChar [Stored event]` instead.
-- That would allow us to ensure that all or none of the events generated by a single
-- command is stored!
data Persistance event = Persistance
    { eventChan :: TChan (Stored event)
    }

persistEvent :: forall event . Persistance event -> event -> STM (Stored event)
persistEvent (Persistance chan) e = do
    let s = unsafePerformIO $ toStored e
    writeTChan chan s
    pure s

data PersistanceError
    = EncodingError String
    deriving (Show, Eq, Typeable, Exception)

filePersistance :: (Show e, ToJSON e, FromJSON e) => FilePath -> IO (Persistance e)
filePersistance fp = do
    chan <- newTChanIO
    f    <- do
        fileExists <- doesFileExist fp
        if fileExists then readFileBinary fp else pure ""
    let events = fmap eitherDecodeStrict . filter (not . BS.null) $ BS.splitWith
            (== fromIntegral (ord '\n'))
            f
    traverse_ (either (throwM . EncodingError) (atomically . writeTChan chan)) events

    -- Duplicate the channel so that old events are not rewritten
    writerChan <- atomically $ dupTChan chan -- should have type `TChar [Stored event]`
    void . async . forever $ do
        s <- atomically $ readTChan writerChan
        let v = encode s <> BL.singleton (fromIntegral $ ord '\n')
        BL.appendFile fp v
    pure $ Persistance chan

noPersistance :: IO (Persistance e)
noPersistance = Persistance <$> newTChanIO

getModel :: Domain model event -> IO model
getModel (Domain _ _ m) = readTVarIO m

-- | Query the model
runQuery :: Domain model event -> (model -> a) -> IO a
runQuery es f = f <$> getModel es

data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

mkId :: MonadIO m => m UUID
mkId = liftIO randomIO

toStored :: MonadIO m => e -> m (Stored e)
toStored e = Stored e <$> getCurrentTime <*> mkId
