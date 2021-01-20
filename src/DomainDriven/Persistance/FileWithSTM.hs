module DomainDriven.Persistance.FileWithSTM where

import           DomainDriven.Internal.Class
import           Prelude
import qualified Data.ByteString                              as BS
import           System.Directory               ( doesFileExist )
import           Data.Typeable
import           Control.Exception
import           Control.Concurrent.STM  hiding ( stateTVar )
import           GHC.Generics                   ( Generic )
import           Data.Char                      ( ord )
import           Data.Aeson
import qualified Data.ByteString.Lazy                         as BL
import           Data.Foldable
import           Data.Traversable
import           Control.Monad.Catch
import           Control.Monad
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           Control.Concurrent.Async       ( async )

data PersistanceError = EncodingError String
    deriving (Show, Eq, Typeable, Exception)


data FileWithSTM model event = FileWithSTM
    { stateTVar :: TVar model
    , eventChan :: TChan (Stored event)
    , eventFile :: FilePath
    , app       :: model -> Stored event -> model
    , seed      :: model
    }
    deriving Generic

createFileWithSTM
    :: (ToJSON event, FromJSON event)
    => FilePath
    -> (model -> Stored event -> model)
    -> model -- ^ initial model
    -> IO (FileWithSTM model event)
createFileWithSTM fp appEvent m0 = do

    chan <- newTChanIO
    tvar <- newTVarIO m0
    f    <- do
        fileExists <- doesFileExist fp
        if fileExists then BS.readFile fp else pure ""
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
    pure $ FileWithSTM tvar chan fp appEvent m0

instance FromJSON event => ReadModel (FileWithSTM model event) where
    type Model (FileWithSTM model event) = model
    type Event (FileWithSTM model event) = event
    applyEvent p = app p
    getModel p = readTVarIO $ stateTVar p
    getEvents p = do
        let fp = eventFile p
        f <- do
            fileExists <- doesFileExist fp
            if fileExists then BS.readFile fp else pure ""
        let events = fmap eitherDecodeStrict . filter (not . BS.null) $ BS.splitWith
                (== fromIntegral (ord '\n'))
                f
        traverse (either (throwM . EncodingError) pure) events


instance (FromJSON event, ToJSON event) => WriteModel (FileWithSTM model event) where
    transactionalUpdate ff evalCmd = atomically $ do
        let tvar = stateTVar ff
        m         <- readTVar tvar
        (r, evs)  <- either throwM pure $ evalCmd m
        storedEvs <- for evs $ \e -> do
            let s = unsafePerformIO $ toStored e
            pure s
        let newModel = foldl' (app ff) m storedEvs
        traverse_ (writeTChan (eventChan ff)) storedEvs
        writeTVar tvar newModel

        pure r
