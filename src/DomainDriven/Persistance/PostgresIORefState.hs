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

newtype EventNumber = EventNumber {unEventNumber :: Int64}
    deriving (Show, Generic)
    deriving newtype (Eq, Ord, Num)

decodeEventRow :: (UUID, UTCTime, e) -> Stored e
decodeEventRow (k, ts, e) = Stored e ts k

data EventRowOut = EventRowOut
    { key         :: UUID
    , timestamp   :: UTCTime
    , event       :: Value
    , eventNumber :: EventNumber
    }
    deriving (Show, Eq, Generic)

data EventRowIn = EventRowIn
    { key       :: UUID
    , timestamp :: UTCTime
    , event     :: Value
    }
    deriving (Show, Eq, Generic)

fromEventRow :: (FromJSON e, MonadThrow m) => EventRowOut -> m (Stored e, EventNumber)
fromEventRow (EventRowOut k ts ev no) = case fromJSON ev of
    Success a -> pure $ (Stored a ts k, no)
    Error err ->
        throwM
            .  EncodingError
            $  "Failed to parse event "
            <> show k
            <> ": "
            <> err
            <> "\nWhen trying to parse:\n"
            <> show ev

--toEventRow :: ToJSON e => Stored e -> EventRowIn
--toEventRow (Stored e ts k) = EventRow k ts (toJSON e)

instance FromRow EventRowOut where
    fromRow =
        EventRowOut
            <$> field
            <*> field
            <*> fieldWith fromJSONField
            <*> fmap EventNumber field

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
    ref <- newIORef (seed', 0)
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
                        \, event_number bigserial\
                        \, timestamp timestamptz not null default now()\
                        \, event jsonb not null\
                        \);"

queryEvents
    :: (Typeable a, FromJSON a)
    => Connection
    -> EventTableName
    -> IO [(Stored a, EventNumber)]
queryEvents conn eventTable = traverse fromEventRow =<< query_
    conn
    ("select * from \"" <> fromString eventTable <> "\" order by event_number")


queryEventsAfter
    :: (Typeable a, FromJSON a)
    => Connection
    -> EventTableName
    -> EventNumber
    -> IO [(Stored a, EventNumber)]
queryEventsAfter conn eventTable (EventNumber lastEvent) =
    traverse fromEventRow =<< query_
        conn
        (  "select * from \""
        <> fromString eventTable
        <> "\" where event_number > "
        <> fromString (show lastEvent)
        <> " order by event_number"
        )


-- queryState' :: (FromJSON a, Typeable a) => Connection -> StateTableName -> IO [a]
-- queryState' conn stateTable = fmap fromStateRow
--     <$> query_ conn ("select * from \"" <> fromString stateTable <> "\"")
--
-- lockState' :: Connection -> StateTableName -> IO Int64
-- lockState' conn stateTable =
--     execute_ conn ("lock \"" <> fromString stateTable <> "\" in exclusive mode")


writeEvents
    :: forall a
     . (FromJSON a, ToJSON a)
    => Connection
    -> EventTableName
    -> [Stored a]
    -> IO EventNumber
writeEvents conn eventTable storedEvents = do
    _ <- executeMany
        conn
        (  "insert into \""
        <> fromString eventTable
        <> "\" (id, timestamp, event) \
            \values (?, ?, ?)"
        )
        (fmap (\x -> (storedUUID x, storedTimestamp x, encode $ storedEvent x))
              storedEvents
        )
    rows <- traverse (fromEventRow @a) =<< query_
        conn
        ("select max(event_number) from \"" <> fromString eventTable <> "\"")
    pure $ foldl' max 0 (fmap snd rows)

-- | Keep the events and state in postgres!
data PostgresEvent model event = PostgresEvent
    { getConnection  :: IO Connection
    , eventTableName :: EventTableName
    , modelIORef     :: IORef (model, EventNumber)
    , app            :: model -> Stored event -> model
    , seed           :: model
    }
    deriving Generic


instance (FromJSON e, Typeable e) => ReadModel (PostgresEvent m e) where
    type Model (PostgresEvent m e) = m
    type Event (PostgresEvent m e) = e
    applyEvent pg = app pg
    getModel pg = do
        (model, lastEventNo) <- readIORef (modelIORef pg)
        conn                 <- getConnection pg
        newEvents            <- queryEventsAfter @e conn (eventTableName pg) lastEventNo
        case newEvents of
            [] -> pure model
            _  -> fst <$> refreshModel pg -- The model must be updated within a transaction

    getEvents pg = do
        conn <- getConnection pg
        fmap fst <$> queryEvents conn (eventTableName pg)

refreshModel :: (Typeable e, FromJSON e) => (PostgresEvent m e) -> IO (m, EventNumber)
refreshModel pg = do
    -- refresh doesn't write any events but changes the state and thus needs a lock
    withExclusiveLock pg $ \conn -> do
        (model    , lastEventNo   ) <- readIORef (modelIORef pg)
        (newEvents, lastNewEventNo) <- do
            (evs, nos) <- unzip <$> queryEventsAfter conn (eventTableName pg) lastEventNo
            pure (evs, foldl' max 0 nos)
        case newEvents of
            [] -> pure (model, lastEventNo)
            _  -> do
                let newModel = foldl' (app pg) model newEvents
                _ <- writeIORef (modelIORef pg) (newModel, lastNewEventNo)
                pure (newModel, lastNewEventNo)

withExclusiveLock :: PostgresEvent m e -> (Connection -> IO a) -> IO a
withExclusiveLock pg f = do
    conn <- getConnection pg
    withTransaction conn $ do
        _ <- execute_
            conn
            ("lock \"" <> fromString (eventTableName pg) <> "\" in exclusive mode")
        f conn

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
            lastEventNo <- writeEvents conn eventTable storedEvs
            _           <- writeIORef (modelIORef pg) (newM, lastEventNo)
            pure a


--migrate1to1
--    :: Connection
--    -> PreviosEventTableName
--    -> EventTableName
--    -> (Value -> Value)
--    -> IO Int64
--migrate1to1 conn prevTName tName f = do
--    _             <- createEventTable conn tName
--    currentEvents <- queryEvents @Value conn prevTName
--    writeEvents conn tName $ fmap (fmap f) currentEvents
