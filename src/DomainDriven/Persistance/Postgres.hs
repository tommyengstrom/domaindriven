module DomainDriven.Persistance.Postgres where

import           DomainDriven.Internal.Class
import           RIO
import           RIO.Time
import           Database.PostgreSQL.Simple
import qualified RIO.ByteString.Lazy                          as BL
import           Data.Aeson
import           Data.UUID
import qualified Database.PostgreSQL.Simple.FromField         as FF
import qualified Database.PostgreSQL.Simple.ToField         as TF


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

-- instance ToJSON a => ToField (EventTable a) where
--   toField = Escape $ encode
decodeEventRow :: FromJSON e => EventTable -> IO (Stored e)
decodeEventRow (EventTable k ts rawEvent) = do
    ev <- either (throwM . EncodingError) pure $ eitherDecodeStrict rawEvent
    pure $ Stored { storedEvent = ev, storedTimestamp = ts, storedUUID = k }

data StateTable = StateTable
    { key       :: UUID
    , timestamp :: UTCTime
    , state     :: ByteString
    }
    deriving (Show, Eq, Ord, Generic, FromRow, ToRow)

decodeStateRow :: FromJSON e => StateTable -> IO e
decodeStateRow (StateTable _ _ rawState) =
    either (throwM . EncodingError) pure $ eitherDecodeStrict rawState


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
    , queryState    = \conn ->
        traverse decodeStateRow =<< query conn "select * from ?" [stateTable]
    , writeState    = \conn (BL.toStrict . encode -> s) -> execute
        conn
        "insert into states(model, state) values (?, ?)"
        ("fixme" :: Text, s)
    , writeEvents   = \conn storedEvents -> executeMany
        conn
        "insert into events (id, timestamp, event) values (?, ?, ?)"
        (fmap (\x -> (storedUUID x, storedTimestamp x, encode $ storedEvent x)) storedEvents)
    , app           = app'
    , seed          = seed'
    }

toEventTable :: ToJSON event => Stored event -> EventTable
toEventTable (Stored ev ts key) =
    EventTable { key = key, timestamp = ts, event = BL.toStrict $ encode ev }


-- | Keep the events and state in postgres!
data PostgresStateAndEvent model event = PostgresStateAndEvent
    { getConnection :: IO Connection
    , queryEvents   :: Connection -> IO [Stored event]
    , queryState    :: Connection -> IO [model]
    , writeState    :: Connection -> model -> IO Int64 -- ^ Insert to write the state
    , writeEvents   :: Connection -> [Stored event] -> IO Int64 -- ^ Insert to write an event
    , app           :: model -> Stored event -> model
    , seed          :: model
    }
    deriving Generic


instance ReadModel (PostgresStateAndEvent m e) where
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
            _ <- (writeState pg) conn s  -- FIXME: Not thread safe!
            pure s


    getEvents pg = do
        conn <- getConnection pg
        (queryEvents pg) conn

instance WriteModel (PostgresStateAndEvent m e) where
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
                    _ <- (writeEvents pg) conn storedEvs
                    _ <- (writeState pg) conn newM
                    pure ret
