-- | Postgres events with state as an IORef
module DomainDriven.Persistance.Postgres.Internal where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable
import Data.Generics.Product
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.IORef
import Data.Int
import Data.Maybe
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
import UnliftIO (MonadUnliftIO (..))
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

data PostgresEvent model index event = PostgresEvent
    { connectionPool :: Pool Connection
    , eventTableName :: EventTableName
    , modelIORef :: IORef (HashMap index (NumberedModel model))
    , app :: model -> Stored event -> model
    , seed :: model
    , chunkSize :: ChunkSize
    -- ^ Number of events read from postgres per batch
    }
    deriving (Generic)

data PostgresEventTrans model index event = PostgresEventTrans
    { transaction :: OngoingTransaction
    , eventTableName :: EventTableName
    , modelIORef :: IORef (HashMap index (NumberedModel model))
    , app :: model -> Stored event -> model
    , seed :: model
    , chunkSize :: ChunkSize
    -- ^ Number of events read from postgres per batch
    }
    deriving (Generic)

instance
    (Hashable index, Show index, FromJSON event)
    => ReadModel (PostgresEvent model index event)
    where
    type Model (PostgresEvent model index event) = model
    type Event (PostgresEvent model index event) = event
    type Index (PostgresEvent model index event) = index
    applyEvent pg = pg ^. field @"app"
    getModel pg = withIOTrans pg . flip getModel'

    getEventList pg index = withResource (connectionPool pg) $ \conn ->
        fmap fst <$> queryEvents conn (pg ^. field @"eventTableName") index

    getEventStream pg = withStreamReadTransaction pg . flip getEventStream'

getEventTableName :: EventTable -> EventTableName
getEventTableName = go 0
  where
    go :: Int -> EventTable -> String
    go i = \case
        MigrateUsing _ u -> go (i + 1) u
        InitialVersion n -> n <> "_v" <> show (i + 1)

-- | Create the table required for storing state and events, if they do not yet exist.
createEventTable :: PostgresEventTrans model index event -> IO ()
createEventTable pgt = do
    void $
        createEventTable'
            (pgt ^. field @"transaction" . to connection)
            (pgt ^. field @"eventTableName")

createEventTable' :: Connection -> EventTableName -> IO Int64
createEventTable' conn eventTable =
    execute_ conn $
        "create table if not exists \""
            <> fromString eventTable
            <> "\" \
               \( id uuid primary key\
               \, index varchar unique\
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
    :: Pool Connection
    -> EventTableName
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent model index event)
postgresWriteModelNoMigration pool eventTable app' seed' = do
    pg <- createPostgresPersistance pool eventTable app' seed'
    withIOTrans pg createEventTable
    pure pg

-- | Setup the persistance model and verify that the tables exist.
postgresWriteModel
    :: forall index model event
     . Pool Connection
    -> EventTable
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent model index event)
postgresWriteModel pool eventTable app' seed' = do
    pg <- createPostgresPersistance pool (getEventTableName eventTable) app' seed'
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
    :: forall event model index
     . Pool Connection
    -> EventTableName
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent model index event)
createPostgresPersistance pool eventTable app' seed' = do
    ref <- newIORef $ HM.empty
    pure $
        PostgresEvent
            { connectionPool = pool
            , eventTableName = eventTable
            , modelIORef = ref
            , app = app'
            , seed = seed'
            , chunkSize = 50
            }

queryEvents
    :: forall a index
     . (Show index, FromJSON a)
    => Connection
    -> EventTableName
    -> index
    -> IO [(Stored a, EventNumber)]
queryEvents conn eventTable index = do
    traverse fromEventRow =<< query_ conn q
  where
    q :: PG.Query
    q =
        "select id, event_number,timestamp,event from \""
            <> fromString eventTable
            <> "\" where index = "
            <> fromString (show index)
            <> " order by event_number"

queryEventsAfter
    :: (Show index, FromJSON a)
    => Connection
    -> EventTableName
    -> index
    -> EventNumber
    -> IO [(Stored a, EventNumber)]
queryEventsAfter conn eventTable index (EventNumber lastEvent) =
    traverse fromEventRow
        =<< query_
            conn
            ( "select id, event_number,timestamp,event from \""
                <> fromString eventTable
                <> "\" where index = "
                <> fromString (show index)
                <> " and event_number > "
                <> fromString (show lastEvent)
                <> " order by event_number"
            )

newtype EventQuery = EventQuery {getPgQuery :: PG.Query}
    deriving (Show, Generic)

mkEventsAfterQuery :: Show index => EventTableName -> index -> EventNumber -> EventQuery
mkEventsAfterQuery eventTable index (EventNumber lastEvent) =
    EventQuery $
        "select id, event_number,timestamp,event from \""
            <> fromString eventTable
            <> "\" where index = "
            <> fromString (show index)
            <> " and event_number > "
            <> fromString (show lastEvent)
            <> " order by event_number"

mkEventQuery :: Show index => EventTableName -> index -> EventQuery
mkEventQuery eventTable index =
    EventQuery $
        "select id, event_number,timestamp,event from \""
            <> fromString eventTable
            <> " where index = "
            <> fromString (show index)
            <> "\" order by event_number"

headMay :: [a] -> Maybe a
headMay = \case
    a : _ -> Just a
    [] -> Nothing

queryHasEventsAfter
    :: Show index
    => Connection
    -> EventTableName
    -> index
    -> EventNumber
    -> IO Bool
queryHasEventsAfter conn eventTable index (EventNumber lastEvent) =
    maybe True fromOnly . headMay <$> query_ conn q
  where
    q :: PG.Query
    q =
        "select count(*) > 0 from \""
            <> fromString eventTable
            <> "\" where index = "
            <> fromString (show index)
            <> " and event_number > "
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
    :: ( FromJSON event
       , Show index
       )
    => PostgresEventTrans model index event
    -> index
    -> Stream IO (Stored event)
getEventStream' pgt index =
    fst
        <$> mkEventStream
            (pgt ^. field @"chunkSize")
            (pgt ^. field @"transaction" . field @"connection")
            (pgt ^. field @"eventTableName" . to (`mkEventQuery` index))

-- | A transaction that is always rolled back at the end.
-- This is useful when using cursors as they can only be used inside a transaction.
withStreamReadTransaction
    :: forall m a model index event
     . ( Stream.MonadAsync m
       , MonadCatch m
       )
    => PostgresEvent model index event
    -> (PostgresEventTrans model index event -> Stream m a)
    -> Stream m a
withStreamReadTransaction pg = Stream.bracket startTrans rollbackTrans
  where
    startTrans :: m (PostgresEventTrans model index event)
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

    rollbackTrans :: PostgresEventTrans model index event -> m ()
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
    :: forall a model index event
     . PostgresEvent model index event
    -> (PostgresEventTrans model index event -> IO a)
    -> IO a
withIOTrans pg f = do
    transactionCompleted <- newIORef False
    (conn, localPool) <- takeResource (connectionPool pg)
    bracket (prepareTransaction conn localPool) (cleanup transactionCompleted) $ \pgt -> do
        a <- f pgt
        writeIORef transactionCompleted True
        pure a
  where
    cleanup :: IORef Bool -> PostgresEventTrans model index event -> IO ()
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
        :: Connection
        -> LocalPool Connection
        -> IO (PostgresEventTrans model index event)
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

getModel'
    :: forall event index model
     . ( FromJSON event
       , Hashable index
       , Show index
       )
    => PostgresEventTrans model index event
    -> index
    -> IO model
getModel' pgt index = do
    NumberedModel model lastEventNo <- getCurrentState pgt index
    hasNewEvents <-
        queryHasEventsAfter
            (pgt ^. field @"transaction" . to connection)
            (pgt ^. field @"eventTableName")
            index
            lastEventNo
    if hasNewEvents
        then fst <$> refreshModel pgt index
        else pure model

refreshModel
    :: forall model index event
     . ( FromJSON event
       , Hashable index
       , Show index
       )
    => PostgresEventTrans model index event
    -> index
    -> IO (model, EventNumber)
refreshModel pg index = do
    -- refresh doesn't write any events but changes the state and thus needs a lock
    exclusiveLock (pg ^. field @"transaction") (pg ^. field @"eventTableName")
    NumberedModel model lastEventNo <- getCurrentState pg index
    let eventStream =
            mkEventStream
                (pg ^. field @"chunkSize")
                (pg ^. field @"transaction" . field @"connection")
                (mkEventsAfterQuery (pg ^. field @"eventTableName") index lastEventNo)

        applyModel
            :: NumberedModel model
            -> (Stored event, EventNumber)
            -> NumberedModel model
        applyModel (NumberedModel m _) (ev, evNumber) =
            NumberedModel ((pg ^. field @"app") m ev) evNumber

    NumberedModel newModel lastNewEventNo <-
        Stream.fold
            ( Fold.foldl'
                applyModel
                (NumberedModel model lastEventNo)
            )
            eventStream

    atomicModifyIORef
        (pg ^. field @"modelIORef")
        (\a -> (HM.insert index (NumberedModel newModel lastNewEventNo) a, ()))
    pure (newModel, lastNewEventNo)

exclusiveLock :: OngoingTransaction -> EventTableName -> IO ()
exclusiveLock (OngoingTransaction conn _) etName =
    void $ execute_ conn ("lock \"" <> fromString etName <> "\" in exclusive mode")

getCurrentState
    :: forall pg index model
     . ( Hashable index
       , HasField' "modelIORef" pg (IORef (HashMap index (NumberedModel model)))
       , HasField' "seed" pg model
       )
    => pg
    -> index
    -> IO (NumberedModel model)
getCurrentState pg index =
    fromMaybe (NumberedModel (pg ^. field' @"seed") 0) . HM.lookup index
        <$> readIORef (pg ^. field' @"modelIORef")

instance
    ( ToJSON event
    , FromJSON event
    , Hashable index
    , Show index
    )
    => WriteModel (PostgresEvent model index event)
    where
    transactionalUpdate pg index cmd = withRunInIO $ \runInIO -> do
        withIOTrans pg $ \pgt -> do
            let eventTable = pg ^. field @"eventTableName"
            exclusiveLock (pgt ^. field @"transaction") eventTable
            m <- getModel' pgt index
            (returnFun, evs) <- runInIO $ cmd m
            NumberedModel m' _ <- getCurrentState pg index
            storedEvs <- traverse toStored evs
            let newM = foldl' (pg ^. field @"app") m' storedEvs
            lastEventNo <-
                writeEvents
                    (pgt ^. field @"transaction" . to connection)
                    eventTable
                    storedEvs
            atomicModifyIORef
                (pg ^. field @"modelIORef")
                (\a -> (HM.insert index (NumberedModel newM lastEventNo) a, ()))
            pure $ returnFun newM
