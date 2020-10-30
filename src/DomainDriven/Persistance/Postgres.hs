module DomainDriven.Persistance.Postgres where

import           DomainDriven.Internal.Class
import           RIO
import           RIO.Time
import           Database.PostgreSQL.Simple
import qualified RIO.ByteString.Lazy                          as BL
import           Data.Aeson
import           Data.UUID (UUID)
import  Database.PostgreSQL.Simple.FromField         as FF
import qualified Database.PostgreSQL.Simple.ToField         as TF
import Database.PostgreSQL.Simple.FromRow as FR



data PersistanceError
    = EncodingError String
    | ValueError String
    deriving (Show, Eq, Typeable, Exception)


type EventTableName = String
type StateTableName = String

decodeEventRow :: (UUID, UTCTime, e) -> Stored e
decodeEventRow (k, ts, e) = Stored e ts k

data EventRow e = EventRow
    { key       :: UUID
    , timestamp :: UTCTime
    , event     :: e
    }
    deriving (Show, Eq, Ord, Generic)

fromEventRow :: EventRow e -> Stored e
fromEventRow (EventRow k ts e) = Stored e ts k

toEventRow :: Stored e -> EventRow e
toEventRow (Stored e ts k) = EventRow k ts e

instance (Typeable e, FromJSON e) => FromRow (EventRow e) where
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


simplePostgres
    :: (FromJSON e, Typeable e, Typeable m, ToJSON e, FromJSON m, ToJSON m)
    => IO Connection
    -> EventTableName
    -> StateTableName
    -> (m -> Stored e -> m)
    -> m
    -> PostgresStateAndEvent m e
simplePostgres getConn eventTable stateTable app' seed' = PostgresStateAndEvent
    { getConnection = getConn
    , createEventTable = \conn ->
        execute_ conn $ "create table \"" <> fromString eventTable <> "\" \
                        \( id uuid primary key\
                        \, timestamp timestamptz not null default now()\
                        \, event jsonb not null\
                        \);"
    , createStateTable = \conn ->
        execute_ conn $ "create table \"" <> fromString stateTable <> "\" \
                \( model text primary key\
                \, timestamp timestamptz not null default now()\
                \, state jsonb not null\
                \);"
    , queryEvents   = \conn ->
        fmap fromEventRow <$> query_ conn
            ("select * from \"" <> fromString eventTable <> "\" order by timestamp")
    , queryState    = \conn ->
        fmap fromStateRow <$> query_ conn
            ("select * from \"" <> fromString stateTable <> "\"")
    , writeState    = \conn (BL.toStrict . encode -> s) -> do
        now <- getCurrentTime
        execute
            conn
            ("insert into \"" <> fromString stateTable <> "\"(model, state, timestamp) \
            \values (?, ?, ?) \
            \on conflict (model) do update set state=(?), timestamp=(?)")
            ("model_id" :: Text, s, now, s, now)
    , writeEvents   = \conn storedEvents -> executeMany
        conn
        ("insert into \"" <> fromString eventTable <> "\" (id, timestamp, event) \
        \values (?, ?, ?)")
        (fmap (\x -> (storedUUID x, storedTimestamp x, encode $ storedEvent x)) storedEvents)
    , app           = app'
    , seed          = seed'
    }



-- | Keep the events and state in postgres!
data PostgresStateAndEvent model event = PostgresStateAndEvent
    { getConnection :: IO Connection
    , createEventTable :: Connection -> IO Int64
    , createStateTable :: Connection -> IO Int64
    , queryEvents   :: Connection -> IO [Stored event]
    , queryState    :: Connection -> IO [model] -- FIXME: One model only!
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
            _ <- writeState pg conn s  -- FIXME: Not thread safe!
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
