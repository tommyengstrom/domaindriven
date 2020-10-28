module DomainDriven.Persistance.Postgres where

import           DomainDriven.Internal.Class
import           RIO
import           RIO.Time
import           Database.PostgreSQL.Simple
import qualified RIO.ByteString.Lazy                          as BL
import           Data.Aeson
import           Data.UUID
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
    :: (FromJSON e, Typeable e, ToJSON e, FromJSON m, ToJSON m)
    => IO Connection
    -> EventTableName
    -> StateTableName
    -> (m -> Stored e -> m)
    -> m
    -> PostgresStateAndEvent m e
simplePostgres getConn eventTable stateTable app' seed' = PostgresStateAndEvent
    { getConnection = getConn
    , queryEvents   = \conn ->
        fmap fromEventRow <$> query_ conn "select * from events order by timestamp"
    , queryState    = \conn ->
        traverse decodeStateRow =<< query_ conn "select * from state"
    , writeState    = \conn (BL.toStrict . encode -> s) -> execute
        conn
        "insert into states(model, state) values (?, ?) \
            \on conflict (model) do update set state=(?)"
        ("fixme" :: Text, s, s)
    , writeEvents   = \conn storedEvents -> executeMany
        conn
        "insert into events (id, timestamp, event) values (?, ?, ?)"
        (fmap (\x -> (storedUUID x, storedTimestamp x, encode $ storedEvent x)) storedEvents)
    , app           = app'
    , seed          = seed'
    }



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
