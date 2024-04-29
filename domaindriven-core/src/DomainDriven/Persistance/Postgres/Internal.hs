-- | Postgres events with state as an IORef
module DomainDriven.Persistance.Postgres.Internal where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable
import Data.Generics.Product
import Data.IORef
import Data.Int
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Cursor qualified as Cursor
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres.Types
import GHC.Generics (Generic)
import Lens.Micro
    ( to
    , (^.)
    )
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream.Prelude (Stream)
import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.Data.Unfold qualified as Unfold
import UnliftIO (MonadUnliftIO (..), concurrently)
import UnliftIO.Pool
    ( LocalPool
    , Pool
    , destroyResource
    , mkDefaultPoolConfig
    , newPool
    , putResource
    , setNumStripes
    , takeResource
    , withResource
    )
import Prelude

data PostgresEvent model event = PostgresEvent
    { connectionPool :: Pool Connection
    , eventTableName :: EventTableName
    , modelIORef :: IORef (NumberedModel model)
    , app :: model -> Stored event -> model
    , seed :: model
    , chunkSize :: ChunkSize
    -- ^ Number of events read from postgres per batch
    , postUpdateHook' :: model -> [Stored event] -> IO ()
    }
    deriving (Generic)

data PostgresEventTrans model event = PostgresEventTrans
    { transaction :: OngoingTransaction
    , eventTableName :: EventTableName
    , modelIORef :: IORef (NumberedModel model)
    , app :: model -> Stored event -> model
    , seed :: model
    , chunkSize :: ChunkSize
    -- ^ Number of events read from postgres per batch
    }
    deriving (Generic)

instance FromJSON e => ReadModel (PostgresEvent m e) where
    type Model (PostgresEvent m e) = m
    type Event (PostgresEvent m e) = e
    applyEvent pg = pg ^. field @"app"
    getModel pg = withIOTrans pg getModel'

    getEventList pg = withResource (connectionPool pg) $ \conn ->
        fmap fst <$> queryEvents conn (pg ^. field @"eventTableName")

    getEventStream pg = withStreamReadTransaction pg getEventStream'

getEventTableName :: EventTable -> EventTableName
getEventTableName = go 0
  where
    go :: Int -> EventTable -> String
    go i = \case
        MigrateUsing _ u -> go (i + 1) u
        InitialVersion n -> n <> "_v" <> show (i + 1)

-- | Create the table required for storing state and events, if they do not yet exist.
createEventTable
    :: (FromJSON e, WriteModel (PostgresEventTrans m e))
    => PostgresEventTrans m e
    -> IO ()
createEventTable pgt = do
    void (getModel pgt)
        `catch` ( const @_ @SqlError $ do
                    let etName = pgt ^. field @"eventTableName"
                    _ <-
                        createEventTable'
                            (pgt ^. field @"transaction" . to connection)
                            etName
                    void $ refreshModel pgt
                )

createEventTable' :: Connection -> EventTableName -> IO Int64
createEventTable' conn eventTable =
    execute_ conn $
        "create table if not exists \""
            <> fromString eventTable
            <> "\" \
               \( id uuid primary key\
               \, event_number bigint not null generated always as identity\
               \, timestamp timestamptz not null default now()\
               \, event jsonb not null\
               \);"

retireTable :: Connection -> EventTableName -> IO ()
retireTable conn tableName = do
    createRetireFunction conn
    void $
        execute_ conn $
            "create trigger retired before insert on \""
                <> fromString tableName
                <> "\" execute procedure retired_table()"

createRetireFunction :: Connection -> IO ()
createRetireFunction conn =
    void
        . execute_ conn
        $ "create or replace function retired_table() returns trigger as \
          \$$ begin raise exception 'Event table has been retired.'; end; $$ \
          \language plpgsql;"

simplePool' :: MonadUnliftIO m => PG.ConnectInfo -> m (Pool Connection)
simplePool' connInfo = simplePool (PG.connect connInfo)

simplePool :: MonadUnliftIO m => IO Connection -> m (Pool Connection)
simplePool getConn = do
    --  Using stripesAndResources because the default is crazy:
    -- https://github.com/scrive/pool/pull/16
    --  "Set number of stripes to number of cores and crash if there are fewer resources"
    let stripesAndResources :: Int
        stripesAndResources = 5
    poolCfg <-
        setNumStripes (Just stripesAndResources)
            <$> mkDefaultPoolConfig (liftIO getConn) (liftIO . PG.close) 1.5 stripesAndResources

    newPool poolCfg

-- | Setup the persistance model and verify that the tables exist.
postgresWriteModelNoMigration
    :: (FromJSON event, WriteModel (PostgresEventTrans model event))
    => Pool Connection
    -> EventTableName
    -> (model -> Stored event -> model)
    -> model
    -> (model -> [Stored event] -> IO ())
    -> IO (PostgresEvent model event)
postgresWriteModelNoMigration pool eventTable app' seed' hook = do
    pg <- createPostgresPersistance pool eventTable app' seed' hook
    withIOTrans pg createEventTable
    pure pg

-- | Setup the persistance model and verify that the tables exist.
postgresWriteModel
    :: Pool Connection
    -> EventTable
    -> (model -> Stored event -> model)
    -> model
    -> (model -> [Stored event] -> IO ())
    -> IO (PostgresEvent model event)
postgresWriteModel pool eventTable app' seed' hook = do
    pg <- createPostgresPersistance pool (getEventTableName eventTable) app' seed' hook
    withIOTrans pg $ \pgt -> runMigrations (pgt ^. field @"transaction") eventTable
    pure pg

newtype Exists = Exists
    { exists :: Bool
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromRow)

runMigrations :: OngoingTransaction -> EventTable -> IO ()
runMigrations trans et = do
    tableExistQuery <-
        query
            conn
            "select exists (select * from information_schema.tables where table_schema='public' and table_name=?)"
            (Only $ getEventTableName et)

    case (et, tableExistQuery) of
        (InitialVersion _, [Only True]) -> pure ()
        (MigrateUsing{}, [Only True]) -> pure ()
        (InitialVersion _, [Only False]) -> createTable
        (MigrateUsing mig prevEt, [Only False]) -> do
            -- Ensure migrations are done up until the previous table
            runMigrations trans prevEt
            -- Then lock lock the previous table before we start
            exclusiveLock trans (getEventTableName prevEt)
            createTable
            mig (getEventTableName prevEt) (getEventTableName et) conn
            retireTable conn (getEventTableName prevEt)
        (_, r) -> fail $ "Unexpected table query result: " <> show r
  where
    conn :: Connection
    conn = connection trans

    createTable :: IO ()
    createTable = do
        let tableName = getEventTableName et
        void $ createEventTable' conn tableName

createPostgresPersistance
    :: forall event model
     . Pool Connection
    -> EventTableName
    -> (model -> Stored event -> model)
    -- ^ Apply event
    -> model
    -- ^ Initial model
    -> (model -> [Stored event] -> IO ())
    -- ^ Post update hook
    -> IO (PostgresEvent model event)
createPostgresPersistance pool eventTable app' seed' hook = do
    ref <- newIORef $ NumberedModel seed' 0
    pure $
        PostgresEvent
            { connectionPool = pool
            , eventTableName = eventTable
            , modelIORef = ref
            , app = app'
            , seed = seed'
            , chunkSize = 50
            , postUpdateHook' = hook
            }

queryEvents
    :: FromJSON a
    => Connection
    -> EventTableName
    -> IO [(Stored a, EventNumber)]
queryEvents conn eventTable = do
    traverse fromEventRow =<< query_ conn q
  where
    q :: PG.Query
    q =
        "select id, event_number,timestamp,event from \""
            <> fromString eventTable
            <> "\" order by event_number"

queryEventsAfter
    :: FromJSON a
    => Connection
    -> EventTableName
    -> EventNumber
    -> IO [(Stored a, EventNumber)]
queryEventsAfter conn eventTable (EventNumber lastEvent) =
    traverse fromEventRow
        =<< query_
            conn
            ( "select id, event_number,timestamp,event from \""
                <> fromString eventTable
                <> "\" where event_number > "
                <> fromString (show lastEvent)
                <> " order by event_number"
            )

newtype EventQuery = EventQuery {getPgQuery :: PG.Query}
    deriving (Show, Generic)

mkEventsAfterQuery :: EventTableName -> EventNumber -> EventQuery
mkEventsAfterQuery eventTable (EventNumber lastEvent) =
    EventQuery $
        "select id, event_number,timestamp,event from \""
            <> fromString eventTable
            <> "\" where event_number > "
            <> fromString (show lastEvent)
            <> " order by event_number"

mkEventQuery :: EventTableName -> EventQuery
mkEventQuery eventTable =
    EventQuery $
        "select id, event_number,timestamp,event from \""
            <> fromString eventTable
            <> "\" order by event_number"

headMay :: [a] -> Maybe a
headMay = \case
    a : _ -> Just a
    [] -> Nothing

queryHasEventsAfter :: Connection -> EventTableName -> EventNumber -> IO Bool
queryHasEventsAfter conn eventTable (EventNumber lastEvent) =
    maybe True fromOnly . headMay <$> query_ conn q
  where
    q :: PG.Query
    q =
        "select count(*) > 0 from \""
            <> fromString eventTable
            <> "\" where event_number > "
            <> fromString (show lastEvent)

writeEvents
    :: forall a
     . ToJSON a
    => Connection
    -> EventTableName
    -> [Stored a]
    -> IO EventNumber
writeEvents conn eventTable storedEvents = do
    _ <-
        executeMany
            conn
            ( "insert into \""
                <> fromString eventTable
                <> "\" (id, timestamp, event) \
                   \values (?, ?, ?)"
            )
            ( fmap
                (\x -> (storedUUID x, storedTimestamp x, encode $ storedEvent x))
                storedEvents
            )
    foldl' max 0 . fmap fromOnly
        <$> query_
            conn
            ("select coalesce(max(event_number),1) from \"" <> fromString eventTable <> "\"")

getEventStream'
    :: FromJSON event => PostgresEventTrans model event -> Stream IO (Stored event)
getEventStream' pgt =
    fst
        <$> mkEventStream
            (pgt ^. field @"chunkSize")
            (pgt ^. field @"transaction" . field @"connection")
            (pgt ^. field @"eventTableName" . to mkEventQuery)

-- | A transaction that is always rolled back at the end.
-- This is useful when using cursors as they can only be used inside a transaction.
withStreamReadTransaction
    :: forall m a model event
     . (Stream.MonadAsync m, MonadCatch m)
    => PostgresEvent model event
    -> (PostgresEventTrans model event -> Stream m a)
    -> Stream m a
withStreamReadTransaction pg = Stream.bracket startTrans rollbackTrans
  where
    startTrans :: m (PostgresEventTrans model event)
    startTrans = liftIO $ do
        (conn, localPool) <- takeResource (connectionPool pg)
        PG.begin conn
        pure $
            PostgresEventTrans
                { transaction = OngoingTransaction conn localPool
                , eventTableName = pg ^. field @"eventTableName"
                , modelIORef = pg ^. field @"modelIORef"
                , app = pg ^. field @"app"
                , seed = pg ^. field @"seed"
                , chunkSize = pg ^. field @"chunkSize"
                }

    rollbackTrans :: PostgresEventTrans model event -> m ()
    rollbackTrans pgt = liftIO $ do
        -- Nothing changes. We just need the transaction to be able to stream events.
        let OngoingTransaction conn localPool = pgt ^. field' @"transaction"

            giveBackConn :: IO ()
            giveBackConn = do
                PG.rollback conn
                putResource localPool conn
        giveBackConn `catchAll` \_ ->
            destroyResource (connectionPool pg) localPool conn

withIOTrans
    :: forall a model event
     . PostgresEvent model event
    -> (PostgresEventTrans model event -> IO a)
    -> IO a
withIOTrans pg f = do
    transactionCompleted <- newIORef False
    (conn, localPool) <- takeResource (connectionPool pg)
    bracket (prepareTransaction conn localPool) (cleanup transactionCompleted) $ \pgt -> do
        a <- f pgt
        writeIORef transactionCompleted True
        pure a
  where
    cleanup :: IORef Bool -> PostgresEventTrans model event -> IO ()
    cleanup transactionCompleted pgt = do
        let OngoingTransaction conn localPool = pgt ^. field' @"transaction"

            giveBackConn :: IO ()
            giveBackConn = do
                readIORef transactionCompleted >>= \case
                    True -> PG.commit conn
                    False -> PG.rollback conn
                putResource localPool conn
        giveBackConn `catchAll` \_ ->
            destroyResource (connectionPool pg) localPool conn

    prepareTransaction
        :: Connection -> LocalPool Connection -> IO (PostgresEventTrans model event)
    prepareTransaction conn localPool = do
        PG.begin conn
        pure $
            PostgresEventTrans
                { transaction = OngoingTransaction conn localPool
                , eventTableName = pg ^. field @"eventTableName"
                , modelIORef = pg ^. field @"modelIORef"
                , app = pg ^. field @"app"
                , seed = pg ^. field @"seed"
                , chunkSize = pg ^. field @"chunkSize"
                }

mkEventStream
    :: FromJSON event
    => ChunkSize
    -> Connection
    -> EventQuery
    -> Stream IO (Stored event, EventNumber)
mkEventStream chunkSize conn q = do
    let step :: Cursor.Cursor -> IO (Maybe (Seq EventRowOut, Cursor.Cursor))
        step cursor = do
            r <- Cursor.foldForward cursor chunkSize (\a r -> pure (a :|> r)) Seq.Empty
            case r of
                Left Seq.Empty -> pure Nothing
                Left a -> pure $ Just (a, cursor)
                Right a -> pure $ Just (a, cursor)

    Stream.bracketIO
        (Cursor.declareCursor conn (getPgQuery q))
        (Cursor.closeCursor)
        ( \cursor ->
            Stream.mapM fromEventRow $
                Stream.unfoldMany Unfold.fromList . fmap toList $
                    Stream.unfoldrM
                        step
                        cursor
        )

getModel' :: forall e m. FromJSON e => PostgresEventTrans m e -> IO m
getModel' pgt = do
    NumberedModel model lastEventNo <- readIORef (pgt ^. field @"modelIORef")
    hasNewEvents <-
        queryHasEventsAfter
            (pgt ^. field @"transaction" . to connection)
            (pgt ^. field @"eventTableName")
            lastEventNo
    if hasNewEvents then fst <$> refreshModel pgt else pure model

refreshModel
    :: forall m e
     . FromJSON e
    => PostgresEventTrans m e
    -> IO (m, EventNumber)
refreshModel pg = do
    -- refresh doesn't write any events but changes the state and thus needs a lock
    exclusiveLock (pg ^. field @"transaction") (pg ^. field @"eventTableName")
    NumberedModel model lastEventNo <- readIORef (pg ^. field @"modelIORef")
    let eventStream =
            mkEventStream
                (pg ^. field @"chunkSize")
                (pg ^. field @"transaction" . field @"connection")
                (mkEventsAfterQuery (pg ^. field @"eventTableName") lastEventNo)

        applyModel :: NumberedModel m -> (Stored e, EventNumber) -> NumberedModel m
        applyModel (NumberedModel m _) (ev, evNumber) =
            NumberedModel ((pg ^. field @"app") m ev) evNumber

    NumberedModel newModel lastNewEventNo <-
        Stream.fold
            ( Fold.foldl'
                applyModel
                (NumberedModel model lastEventNo)
            )
            eventStream

    _ <- writeIORef (pg ^. field @"modelIORef") $ NumberedModel newModel lastNewEventNo
    pure (newModel, lastNewEventNo)

exclusiveLock :: OngoingTransaction -> EventTableName -> IO ()
exclusiveLock (OngoingTransaction conn _) etName =
    void $ execute_ conn ("lock \"" <> fromString etName <> "\" in exclusive mode")

instance (ToJSON e, FromJSON e) => WriteModel (PostgresEvent m e) where
    postUpdateHook pg m e = liftIO $ (pg ^. field @"postUpdateHook'") m e

    transactionalUpdate pg cmd = withRunInIO $ \runInIO ->
        withIOTrans pg $ \pgt -> do
            let eventTable = pg ^. field @"eventTableName"
            exclusiveLock (pgt ^. field @"transaction") eventTable
            m <- getModel' pgt
            (returnFun, evs) <- runInIO $ cmd m
            NumberedModel m' _ <- readIORef (pg ^. field @"modelIORef")
            storedEvs <- traverse toStored evs
            newNumberedModel <-
                uncurry NumberedModel
                    <$> concurrently
                        ( Stream.fold
                            (Fold.foldl' (pg ^. field @"app") m')
                            (Stream.fromList storedEvs)
                        )
                        ( writeEvents
                            (pgt ^. field @"transaction" . to connection)
                            eventTable
                            storedEvs
                        )
            _ <- writeIORef (pg ^. field @"modelIORef") newNumberedModel
            pure $ (model newNumberedModel, storedEvs, returnFun)
