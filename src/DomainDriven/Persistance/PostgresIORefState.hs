module DomainDriven.Persistance.PostgresIORefState where

import           DomainDriven.Internal.Class
import           Prelude
import           Data.Time
import           Database.PostgreSQL.Simple
import           Data.Int
import           Data.String
import           Control.Monad
import           Data.List                      ( foldl' )
import           Data.Typeable
import           Control.Monad.Catch
import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import           Data.Aeson
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple.FromField         as FF
import           Database.PostgreSQL.Simple.FromRow           as FR
import           Data.IORef



data PersistanceError
    = EncodingError String
    | ValueError String
    deriving (Show, Eq, Typeable, Exception)


type PreviosEventTableName = String
type EventTableName = String

decodeEventRow :: (UUID, UTCTime, e) -> Stored e
decodeEventRow (k, ts, e) = Stored e ts k

data EventRow = EventRow
    { key       :: UUID
    , timestamp :: UTCTime
    , event     :: Value
    }
    deriving (Show, Eq, Generic)

fromEventRow :: (MonadThrow m, FromJSON e) => EventRow -> m (Stored e)
fromEventRow (EventRow k ts ev) = case fromJSON ev of
    Success a -> pure $ Stored a ts k
    Error err ->
        throwM
            .  EncodingError
            $  "Failed to parse event "
            <> show k
            <> ": "
            <> err
            <> "\nWhen trying to parse:\n"
            <> show ev

toEventRow :: ToJSON e => Stored e -> EventRow
toEventRow (Stored e ts k) = EventRow k ts (toJSON e)

instance FromRow EventRow where
    fromRow = EventRow <$> field <*> field <*> fieldWith fromJSONField

data StateRow m = StateRow
    { modelName :: Text
    , timestamp :: UTCTime
    , state     :: m
    }
    deriving (Show, Eq, Ord, Generic)

instance (Typeable m, FromJSON m) => FromRow (StateRow m) where
    fromRow = StateRow <$> field <*> field <*> fieldWith fromJSONField

fromStateRow :: StateRow s -> s
fromStateRow = state

-- | Create the table required for storing state and events, if they do not yet exist.
createTables :: (FromJSON e, Typeable e) => PostgresEvent m e -> IO ()
createTables runner = do
    conn <- getConnection runner
    void (getModel runner)
        `catch` (const @_ @SqlError $ do
                    _ <- createEventTable conn (eventTableName runner)
                    void $ getModel runner
                )

-- | Setup the persistance model and verify that the tables exist.
simplePostgres
    :: (FromJSON e, Typeable e, Typeable m, ToJSON e, FromJSON m, ToJSON m)
    => IO Connection
    -> EventTableName
    -> (m -> Stored e -> m)
    -> m
    -> IO (PostgresEvent m e)
simplePostgres getConn eventTable app' seed' = do
    runner <- createPostgresPersistance getConn eventTable app' seed'
    createTables runner
    pure runner

createPostgresPersistance
    :: forall event model
     . (FromJSON event, Typeable event, ToJSON event)
    => IO Connection
    -> EventTableName
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent model event)
createPostgresPersistance getConn eventTable app' seed' = do
    ref <- newIORef seed'
    pure $ PostgresEvent { getConnection  = getConn
                         , eventTableName = eventTable
                         , modelIORef     = ref
                         , app            = app'
                         , seed           = seed'
                         }


createEventTable :: Connection -> EventTableName -> IO Int64
createEventTable conn eventTable =
    execute_ conn
        $ "create table \""
        <> fromString eventTable
        <> "\" \
                        \( id uuid primary key\
                        \, timestamp timestamptz not null default now()\
                        \, event jsonb not null\
                        \);"

queryEvents :: (Typeable a, FromJSON a) => Connection -> EventTableName -> IO [Stored a]
queryEvents conn eventTable = traverse fromEventRow =<< query_
    conn
    ("select * from \"" <> fromString eventTable <> "\" order by timestamp")

-- queryState' :: (FromJSON a, Typeable a) => Connection -> StateTableName -> IO [a]
-- queryState' conn stateTable = fmap fromStateRow
--     <$> query_ conn ("select * from \"" <> fromString stateTable <> "\"")
--
-- lockState' :: Connection -> StateTableName -> IO Int64
-- lockState' conn stateTable =
--     execute_ conn ("lock \"" <> fromString stateTable <> "\" in exclusive mode")

writeEvents :: ToJSON a => Connection -> EventTableName -> [Stored a] -> IO Int64
writeEvents conn eventTable storedEvents = executeMany
    conn
    (  "insert into \""
    <> fromString eventTable
    <> "\" (id, timestamp, event) \
        \values (?, ?, ?)"
    )
    (fmap (\x -> (storedUUID x, storedTimestamp x, encode $ storedEvent x)) storedEvents)

-- | Keep the events and state in postgres!
data PostgresEvent model event = PostgresEvent
    { getConnection  :: IO Connection
    , eventTableName :: EventTableName
    , modelIORef     :: IORef model
    , app            :: model -> Stored event -> model
    , seed           :: model
    }
    deriving Generic


instance (FromJSON e, Typeable e) => ReadModel (PostgresEvent m e) where
    type Model (PostgresEvent m e) = m
    type Event (PostgresEvent m e) = e
    applyEvent pg = app pg
    getModel pg = readIORef (modelIORef pg)
    getEvents pg = do
        conn <- getConnection pg
        queryEvents conn (eventTableName pg)

lockTable :: Connection -> EventTableName -> IO ()
lockTable conn eventTable =
    void $ execute_ conn ("lock \"" <> fromString eventTable <> "\" in exclusive mode")

instance (ToJSON e, FromJSON e, Typeable e) => WriteModel (PostgresEvent m e) where
    transactionalUpdate pg cmd = do
        conn <- getConnection pg
        withTransaction conn $ do
            let eventTable = eventTableName pg
            _ <- execute_
                conn
                ("lock \"" <> fromString eventTable <> "\" in exclusive mode")
            (a, evs)  <- cmd
            m         <- getModel pg
            storedEvs <- traverse toStored evs
            let newM = foldl' (app pg) m storedEvs
            _ <- writeEvents conn eventTable storedEvs
            _ <- writeIORef (modelIORef pg) newM
            pure a


migrate1to1
    :: Connection
    -> PreviosEventTableName
    -> EventTableName
    -> (Value -> Value)
    -> IO Int64
migrate1to1 conn prevTName tName f = do
    _             <- createEventTable conn tName
    currentEvents <- queryEvents @Value conn prevTName
    writeEvents conn tName $ fmap (fmap f) currentEvents
