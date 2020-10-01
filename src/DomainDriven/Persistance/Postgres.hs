module DomainDriven.Persistance.Postgres where

import           DomainDriven.Internal.Class
import           RIO
import           RIO.Time
import           Database.PostgreSQL.Simple
import           Data.Aeson
import           Data.UUID
import           RIO.List                       ( headMaybe )


data PersistanceError
    = EncodingError String
    | ValueError String
    deriving (Show, Eq, Typeable, Exception)


type EventTableName = String
type StateTableName = String

data EventTable = EventTable
    { key       :: UUID
    , timestamp :: UTCTime
    , event     :: ByteString
    }
    deriving (Show, Eq, Ord, Generic, FromRow, ToRow)

simplePostgres
    :: (FromJSON e, ToJSON e, FromJSON m, ToJSON m)
    => IO Connection
    -> EventTableName
    -> StateTableName
    -> (m -> Stored e -> m)
    -> m
    -> PostgresStateAndEvent m e
simplePostgres getConn eventTable stateTable app' seed' = PostgresStateAndEvent
    { getConnection = getConn
    , queryEvents   = \conn ->
        traverse decodeEventRow =<< query conn "select * from ?" [eventTable]
    , queryState    = \conn -> fmap fromOnly <$> query conn "select * from ?" [stateTable]
    , writeState    = undefined
    , writeEvents   = undefined
    , app           = app'
    , seed          = seed'
    }

decodeEventRow :: FromJSON e => EventTable -> IO (Stored e)
decodeEventRow (EventTable k ts rawEvent) = do
    ev <- either (throwM . EncodingError) pure $ eitherDecodeStrict rawEvent
    pure $ Stored { storedEvent = ev, storedTimestamp = ts, storedUUID = k }

-- | Keep the events and state in postgres!
data PostgresStateAndEvent model event = PostgresStateAndEvent
    { getConnection :: IO Connection
    , queryEvents   :: Connection -> IO [Stored event]
    , queryState    :: Connection -> IO [model]
    , writeState    :: Query -- ^ Insert to write the state
    , writeEvents   :: Query -- ^ Insert to write an event
    , app           :: model -> Stored event -> model
    , seed          :: model
    }
    deriving Generic


instance (FromRow m, FromRow (Stored e), ToRow m)
        => ReadModel (PostgresStateAndEvent m e) where
    type Model (PostgresStateAndEvent m e) = m
    type Event (PostgresStateAndEvent m e) = e
    applyEvent pg = app pg
    getModel pg = do
        conn <- getConnection pg
        r    <- (queryState pg) conn
            `catch` const @_ @ResultError (pure <$> recalculateState conn)
        case r of
            []  -> recalculateState conn
            [x] -> pure x
            xs ->
                throwM
                    $  ValueError
                    $  "Invalid state query. Found "
                    <> show (length xs)
                    <> " states"
      where
        recalculateState :: Connection -> IO m
        recalculateState conn = do
            s <- foldl' (app pg) (seed pg) <$> getEvents pg
            _ <- query conn (writeState pg) s :: IO [m] -- FIXME: Not thread safe!
            pure s


    getEvents pg = do
        conn <- getConnection pg
        (queryEvents pg) conn

instance (FromRow m, ToRow m, FromRow (Stored e), ToRow (Stored e))
        => WriteModel (PostgresStateAndEvent m e) where
    transactionalUpdate pg evalCmd = do
        conn <- getConnection pg
        withTransaction conn $ do
            m <- getModel pg
            case evalCmd m of
                Left  err        -> throwM err
                Right (ret, evs) -> do
                    storedEvs <- traverse toStored evs
                    let newM = foldl' (app pg) m storedEvs
                        -- FIXME: Ensure the events are applied in the correct order!
                    _ <-
                        traverse (query conn (writeEvents pg)) storedEvs :: IO
                            [[Stored e]]
                    _ <- query conn (writeState pg) newM :: IO [m]
                    pure ret
