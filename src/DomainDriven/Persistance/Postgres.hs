module DomainDriven.Persistance.Postgres where

import           DomainDriven.Internal.Class
import           Prelude
import           Data.Time
import           Database.PostgreSQL.Simple
import qualified Data.ByteString.Lazy                         as BL
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



data PersistanceError
    = EncodingError String
    | ValueError String
    deriving (Show, Eq, Typeable, Exception)


type PreviosEventTableName = String
type EventTableName = String
type StateTableName = String

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
createTables :: PostgresStateAndEvent m e -> IO ()
createTables runner = do
    conn <- getConnection runner
    void (getModel runner)
        `catch` (const @_ @SqlError $ do
                    _ <- createEventTable runner conn
                    _ <- createStateTable runner conn
                    void (getModel runner)
                )

-- | Setup the persistance model and verify that the tables exist.
simplePostgres
    :: (FromJSON e, Typeable e, Typeable m, ToJSON e, FromJSON m, ToJSON m)
    => IO Connection
    -> EventTableName
    -> StateTableName
    -> (m -> Stored e -> m)
    -> m
    -> IO (PostgresStateAndEvent m e)
simplePostgres getConn eventTable stateTable app' seed' = do
    let runner = createPostgresPersistance getConn eventTable stateTable app' seed'
    createTables runner
    pure runner

createPostgresPersistance
    :: (FromJSON e, Typeable e, Typeable m, ToJSON e, FromJSON m, ToJSON m)
    => IO Connection
    -> EventTableName
    -> StateTableName
    -> (m -> Stored e -> m)
    -> m
    -> PostgresStateAndEvent m e
createPostgresPersistance getConn eventTable stateTable app' seed' =
    PostgresStateAndEvent { getConnection    = getConn
                          , createEventTable = flip createEventTable' eventTable
                          , clearStateTable  = flip clearStateTable' stateTable
                          , createStateTable = flip createStateTable' stateTable
                          , queryEvents      = flip queryEvents' eventTable
                          , queryState       = flip queryState' stateTable
                          , lockState        = flip lockState' stateTable
                          , writeState       = \conn s -> writeState' conn stateTable s
                          , writeEvents = \conn evs -> writeEvents' conn eventTable evs
                          , app              = app'
                          , seed             = seed'
                          }

clearStateTable' :: Connection -> StateTableName -> IO Int64
clearStateTable' conn stateTable =
    execute_ conn $ "delete from \"" <> fromString stateTable <> "\""

createEventTable' :: Connection -> EventTableName -> IO Int64
createEventTable' conn eventTable =
    execute_ conn
        $ "create table \""
        <> fromString eventTable
        <> "\" \
                        \( id uuid primary key\
                        \, timestamp timestamptz not null default now()\
                        \, event jsonb not null\
                        \);"

createStateTable' :: Connection -> StateTableName -> IO Int64
createStateTable' conn stateTable =
    execute_ conn
        $ "create table \""
        <> fromString stateTable
        <> "\" \
        \( model text primary key\
        \, timestamp timestamptz not null default now()\
        \, state jsonb not null\
        \);"

queryEvents' :: (Typeable a, FromJSON a) => Connection -> EventTableName -> IO [Stored a]
queryEvents' conn eventTable = traverse fromEventRow =<< query_
    conn
    ("select * from \"" <> fromString eventTable <> "\" order by timestamp")

queryState' :: (FromJSON a, Typeable a) => Connection -> StateTableName -> IO [a]
queryState' conn stateTable = fmap fromStateRow
    <$> query_ conn ("select * from \"" <> fromString stateTable <> "\"")

lockState' :: Connection -> StateTableName -> IO Int64
lockState' conn stateTable =
    execute_ conn ("lock \"" <> fromString stateTable <> "\" in exclusive mode")

writeEvents' :: ToJSON a => Connection -> EventTableName -> [Stored a] -> IO Int64
writeEvents' conn eventTable storedEvents = executeMany
    conn
    (  "insert into \""
    <> fromString eventTable
    <> "\" (id, timestamp, event) \
        \values (?, ?, ?)"
    )
    (fmap (\x -> (storedUUID x, storedTimestamp x, encode $ storedEvent x)) storedEvents)

writeState' :: ToJSON a => Connection -> StateTableName -> a -> IO Int64
writeState' conn stateTable (BL.toStrict . encode -> s) = do
    now <- getCurrentTime
    execute
        conn
        ("insert into \""
        <> fromString stateTable
        <> "\"(model, state, timestamp) \
    \values (?, ?, ?) \
    \on conflict (model) do update set state=(?), timestamp=(?)"
        )
        ("model_id" :: Text, s, now, s, now)

-- | Keep the events and state in postgres!
data PostgresStateAndEvent model event = PostgresStateAndEvent
    { getConnection    :: IO Connection
    , createEventTable :: Connection -> IO Int64
    , createStateTable :: Connection -> IO Int64
    , clearStateTable  :: Connection -> IO Int64
    , queryEvents      :: Connection -> IO [Stored event]
    , queryState       :: Connection -> IO [model] -- FIXME: One model only!
    , lockState        :: Connection -> IO Int64 -- ^ Get an exclusive lock on the model
    , writeState       :: Connection -> model -> IO Int64 -- ^ Insert to write the state
    , writeEvents      :: Connection -> [Stored event] -> IO Int64 -- ^ Insert to write an event
    , app              :: model -> Stored event -> model
    , seed             :: model
    }
    deriving Generic


instance ReadModel (PostgresStateAndEvent m e) where
    type Model (PostgresStateAndEvent m e) = m
    type Event (PostgresStateAndEvent m e) = e
    applyEvent pg = app pg
    getModel pg = do
        conn <- getConnection pg
        r    <- queryState pg conn
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
            _ <- writeState pg conn s  -- FIXME: Not thread safe!
            pure s


    getEvents pg = do
        conn <- getConnection pg
        (queryEvents pg) conn

instance WriteModel (PostgresStateAndEvent m e) where
    transactionalUpdate pg cmd = do
        conn <- getConnection pg
        withTransaction conn $ do
            _         <- lockState pg conn
            (a, evs)  <- cmd
            m         <- getModel pg
            storedEvs <- traverse toStored evs
            let newM = foldl' (app pg) m storedEvs
            _ <- (writeEvents pg) conn storedEvs
            _ <- (writeState pg) conn newM
            pure a


migrate1to1
    :: Connection
    -> PreviosEventTableName
    -> EventTableName
    -> (Value -> Value)
    -> IO Int64
migrate1to1 conn prevTName tName f = do
    _             <- createEventTable' conn tName
    currentEvents <- queryEvents' @Value conn prevTName
    writeEvents' conn tName $ fmap (fmap f) currentEvents
