module DomainDriven.Persistance.FileAndSTM where

import           DomainDriven.Internal.Class
import           RIO
import qualified RIO.ByteString                               as BS
import           System.Directory               ( doesFileExist )
import           Data.Char                      ( ord )
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           Data.Aeson
import qualified RIO.ByteString.Lazy                          as BL


data PersistanceError = EncodingError String
    deriving (Show, Eq, Typeable, Exception)

-- | Streaming json storage to disk with STM state.
data FileAndSTM model event = FileAndSTM
    { eventChan :: TChan (Stored event)
    , stateTVar :: TVar model
    , eventFile :: FilePath
    }
    deriving Generic

createFileAndSTM
    :: (ToJSON event, FromJSON event)
    => FilePath
    -> (model -> Stored event -> model)
    -> model -- ^ initial model
    -> IO (DomainModel (FileAndSTM model event) model event)
createFileAndSTM fp appEvent m0 = do
    chan <- newTChanIO
    tvar <- newTVarIO m0
    f    <- do
        fileExists <- doesFileExist fp
        if fileExists then readFileBinary fp else pure ""
    let events = fmap eitherDecodeStrict . filter (not . BS.null) $ BS.splitWith
            (== fromIntegral (ord '\n'))
            f
    -- Read all existing events and apply to the model
    traverse_
        (either (throwM . EncodingError) (atomically . modifyTVar tvar . flip appEvent))
        events

    -- Create the thread that will persist events to file
    void . async . forever $ do
        s <- atomically $ readTChan chan
        let v = encode s <> BL.singleton (fromIntegral $ ord '\n')
        BL.appendFile fp v

    pure $ DomainModel (FileAndSTM chan tvar fp) appEvent

instance FromJSON event => PersistanceHandler (FileAndSTM model event) model event where
    getModel (FileAndSTM _ tvar _) = readTVarIO tvar
    getEvents (FileAndSTM _ _ fp) = do
        f <- do
            fileExists <- doesFileExist fp
            if fileExists then readFileBinary fp else pure ""
        let events = fmap eitherDecodeStrict . filter (not . BS.null) $ BS.splitWith
                (== fromIntegral (ord '\n'))
                f
        traverse (either (throwM . EncodingError) pure) events

    transactionalUpdate (FileAndSTM chan tvar _) appEvent evalCmd = do
        atomically $ do
            m         <- readTVar tvar
            (r, evs)  <- either throwM pure $ evalCmd m
            storedEvs <- for evs $ \e -> do
                let s = unsafePerformIO $ toStored e
                writeTChan chan s
                pure s
            let newModel = foldl' appEvent m storedEvs
            writeTVar tvar newModel
            pure r
