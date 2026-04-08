{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module DomainDriven.Persistance.Postgres.Beam
    ( BeamProjectionSpec (..)
    , PostgresBeam (..)
    , postgresBeamBackend
    , rebuildProjection
    , runBeamCmd
    , runBeamPg
    )
where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.Hashable (hash)
import Data.Int
import Data.Maybe (listToMaybe)
import Data.Pool.Introspection as Pool
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import Database.Beam
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate (migrationBackend)
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Cursor qualified as Cursor
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres.Internal
    ( LogEntry (..)
    , OneLineCallStack (..)
    , getEventTableName
    , mkEventQuery
    , mkEventStream
    , queryEvents
    , runMigrations
    , writeEvents
    )
import DomainDriven.Persistance.Postgres.Types
import GHC.Stack
import Lens.Micro
    ( (^.)
    )
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream.Prelude (Stream)
import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.Data.Unfold qualified as Unfold
import System.IO (hPutStrLn)
import UnliftIO
import Prelude

data BeamProjectionSpec db model index event = BeamProjectionSpec
    { checkedDb :: CheckedDatabaseSettings Postgres db
    , dropProjection :: Connection -> IO ()
    , applyModelEvent :: model -> Stored event -> model
    , loadProjectionModel :: DatabaseSettings Postgres db -> index -> Pg model
    , projectStoredEvent :: DatabaseSettings Postgres db -> index -> Stored event -> Pg ()
    }

data PostgresBeam index db model event = PostgresBeam
    { connectionPool :: Pool Connection
    , eventTableName :: EventTableName
    , projectionSpec :: BeamProjectionSpec db model index event
    , chunkSize :: ChunkSize
    , updateHook
        :: PostgresBeam index db model event
        -> index
        -> model
        -> [Stored event]
        -> IO ()
    , logger :: LogEntry -> IO ()
    }
    deriving (Generic)

data PostgresBeamTrans index db model event = PostgresBeamTrans
    { transaction :: OngoingTransaction
    , eventTableName :: EventTableName
    , projectionSpec :: BeamProjectionSpec db model index event
    , chunkSize :: ChunkSize
    , logger :: LogEntry -> IO ()
    }
    deriving (Generic)

newtype ProjectionMetaRow = ProjectionMetaRow
    { projectionEventTableName :: String
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromRow)

data IndexedEventRowOut = IndexedEventRowOut
    { key :: UUID
    , streamIndex :: Text
    , commitNumber :: EventNumber
    , timestamp :: UTCTime
    , event :: Value
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromRow)

projectionMetaTableName :: EventTableName -> EventTableName
projectionMetaTableName eventTable = eventTable <> "_projection_meta"

unCheckedDb :: Database Postgres db => BeamProjectionSpec db model index event -> DatabaseSettings Postgres db
unCheckedDb = unCheckDatabase . checkedDb

defaultLogger :: LogEntry -> IO ()
defaultLogger = \case
    e@(DbTransactionDuration dt _) -> when (dt > 1) $ putStrLn $ "[DomainDriven] " <> show e
    e@(EventTableLockDuration dt _) -> when (dt > 0.5) $ putStrLn $ "[DomainDriven] " <> show e
    EventTableMigrationDuration dt etName ->
        putStrLn $ "[DomainDriven] migration of " <> etName <> " completed in " <> show dt
    e@(WaitForConnectionDuration dt _) -> when (dt > 0.5) $ putStrLn $ "[DomainDriven] " <> show e

postgresBeamBackend
    :: forall index db model event
     . ( Database Postgres db
       , HasCallStack
       , FromJSON event
       , IsPgIndex index
       )
    => Pool Connection
    -> EventTable
    -> BeamProjectionSpec db model index event
    -> IO (PostgresBeam index db model event)
postgresBeamBackend pool eventTable spec = do
    let eventTableName = getEventTableName eventTable
    beam <- createPostgresBeam pool eventTableName spec
    withBeamTrans beam $ \pbt -> do
        runMigrations (pbt ^. field @"logger") (pbt ^. field @"transaction") eventTable
        ensureProjectionReady pbt
    pure beam

createPostgresBeam
    :: Pool Connection
    -> EventTableName
    -> BeamProjectionSpec db model index event
    -> IO (PostgresBeam index db model event)
createPostgresBeam pool eventTableName spec =
    pure $
        PostgresBeam
            { connectionPool = pool
            , eventTableName = eventTableName
            , projectionSpec = spec
            , chunkSize = 50
            , updateHook = \_ _ _ _ -> pure ()
            , logger = defaultLogger
            }

runBeamPg
    :: forall index db model event a
     . PostgresBeam index db model event
    -> Pg a
    -> IO a
runBeamPg beam beamQuery = withResource (beam ^. field @"connectionPool") $ \conn ->
    runBeamPostgres (resource conn) beamQuery

runBeamCmd
    :: forall index db model event a
     . ( HasCallStack
       , Database Postgres db
       , IsPgIndex index
       , ToJSON event
       )
    => PostgresBeam index db model event
    -> index
    -> ((forall x. Pg x -> IO x) -> IO (Pg a, [event]))
    -> IO a
runBeamCmd beam index cmd = withFrozenCallStack do
    (result, model, storedEvents) <- transactionalBeamUpdate beam index cmd
    _ <- async $ updateHook beam beam index model storedEvents `catchAny` \e ->
        hPutStrLn stderr $ "[DomainDriven] postUpdateHook failed: " <> displayException e
    pure result

transactionalBeamUpdate
    :: forall index db model event a
     . ( HasCallStack
       , Database Postgres db
       , IsPgIndex index
       , ToJSON event
       )
    => PostgresBeam index db model event
    -> index
    -> ((forall x. Pg x -> IO x) -> IO (Pg a, [event]))
    -> IO (a, model, [Stored event])
transactionalBeamUpdate beam index cmd =
    withBeamTrans beam $ \pbt -> withScopedLock pbt index $ do
        let runPgInTx :: forall x. Pg x -> IO x
            runPgInTx = runBeamPostgres (transactionConnection pbt)
        (returnQuery, events) <- cmd runPgInTx
        storedEvents <- traverse toStored events
        writeAndProjectStoredEvents pbt index storedEvents
        result <- runPgInTx returnQuery
        model <- loadModel pbt index
        pure (result, model, storedEvents)

writeAndProjectStoredEvents
    :: forall index db model event
     . ( Database Postgres db
       , IsPgIndex index
       , ToJSON event
       )
    => PostgresBeamTrans index db model event
    -> index
    -> [Stored event]
    -> IO ()
writeAndProjectStoredEvents _ _ [] = pure ()
writeAndProjectStoredEvents pbt index storedEvents = do
    void $
        writeEvents
            (transactionConnection pbt)
            (pbt ^. field @"eventTableName")
            index
            storedEvents
    for_ storedEvents $
        runBeamPostgres (transactionConnection pbt)
            . projectStoredEvent (pbt ^. field @"projectionSpec")
                (unCheckedDb $ pbt ^. field @"projectionSpec")
                index

loadModel
    :: Database Postgres db
    => PostgresBeamTrans index db model event
    -> index
    -> IO model
loadModel pbt index =
    runBeamPostgres (transactionConnection pbt) $
        loadProjectionModel
            (pbt ^. field @"projectionSpec")
            (unCheckedDb $ pbt ^. field @"projectionSpec")
            index

rebuildProjection
    :: forall index db model event
     . ( Database Postgres db
       , FromJSON event
       , IsPgIndex index
       )
    => PostgresBeam index db model event
    -> IO ()
rebuildProjection beam = withBeamTrans beam rebuildProjectionInTransaction

ensureProjectionReady
    :: forall index db model event
     . ( Database Postgres db
       , FromJSON event
       , IsPgIndex index
       )
    => PostgresBeamTrans index db model event
    -> IO ()
ensureProjectionReady pbt = do
    ensureProjectionMetaTable pbt
    projectionTableMatches <- currentProjectionEventTable pbt
    verificationResult <-
        runBeamPostgres (transactionConnection pbt) $
            verifySchema migrationBackend (checkedDb $ pbt ^. field @"projectionSpec")
    unless
        ( schemaIsValid verificationResult
            && projectionTableMatches == Just (pbt ^. field @"eventTableName")
        )
        (rebuildProjectionInTransaction pbt)
  where
    schemaIsValid :: VerificationResult -> Bool
    schemaIsValid = \case
        VerificationSucceeded -> True
        _ -> False

rebuildProjectionInTransaction
    :: forall index db model event
     . ( Database Postgres db
       , FromJSON event
       , IsPgIndex index
       )
    => PostgresBeamTrans index db model event
    -> IO ()
rebuildProjectionInTransaction pbt = do
    let conn = transactionConnection pbt
        spec = pbt ^. field @"projectionSpec"
    ensureProjectionMetaTable pbt
    lockEventTableExclusive conn (pbt ^. field @"eventTableName")
    dropProjection spec conn
    runBeamPostgres conn $ createSchema migrationBackend (checkedDb spec)
    Stream.fold
        (Fold.drainMapM projectOne)
        (mkAllEventStream (pbt ^. field @"chunkSize") conn (pbt ^. field @"eventTableName"))
    setCurrentProjectionEventTable pbt
  where
    projectOne :: (index, Stored event, EventNumber) -> IO ()
    projectOne (index, storedEvent, _eventNumber) =
        runBeamPostgres (transactionConnection pbt) $
            projectStoredEvent
                (pbt ^. field @"projectionSpec")
                (unCheckedDb $ pbt ^. field @"projectionSpec")
                index
                storedEvent

ensureProjectionMetaTable :: PostgresBeamTrans index db model event -> IO ()
ensureProjectionMetaTable pbt =
    void $
        execute_
            (transactionConnection pbt)
            ( "create table if not exists "
                <> quoteIdent (projectionMetaTableName $ pbt ^. field @"eventTableName")
                <> " (projection_name varchar primary key, event_table_name varchar not null)"
            )

currentProjectionEventTable
    :: PostgresBeamTrans index db model event
    -> IO (Maybe EventTableName)
currentProjectionEventTable pbt =
    fmap projectionEventTableName . listToMaybe
        <$> query_
            (transactionConnection pbt)
            ( "select event_table_name from "
                <> quoteIdent (projectionMetaTableName $ pbt ^. field @"eventTableName")
                <> " where projection_name = 'projection'"
            )

setCurrentProjectionEventTable
    :: PostgresBeamTrans index db model event
    -> IO ()
setCurrentProjectionEventTable pbt = do
    void $
        execute
            (transactionConnection pbt)
            ( "insert into "
                <> quoteIdent (projectionMetaTableName $ pbt ^. field @"eventTableName")
                <> " (projection_name, event_table_name) values ('projection', ?)"
                <> " on conflict (projection_name) do update set event_table_name = excluded.event_table_name"
            )
            (Only $ pbt ^. field @"eventTableName")

transactionConnection :: PostgresBeamTrans index db model event -> Connection
transactionConnection =
    resource . connectionResource . transaction

withBeamTrans
    :: forall a index db model event
     . HasCallStack
    => PostgresBeam index db model event
    -> (PostgresBeamTrans index db model event -> IO a)
    -> IO a
withBeamTrans beam action = do
    transactionCompleted <- newIORef False
    (connR, localPool) <- do
        t0 <- getCurrentTime
        resource' <- takeResource (beam ^. field @"connectionPool")
        t1 <- getCurrentTime
        beam ^. field @"logger" $
            WaitForConnectionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
        pure resource'
    UnliftIO.bracket (prepareTransaction connR localPool) (cleanup transactionCompleted) $ \pbt -> do
        result <- action pbt
        writeIORef transactionCompleted True
        pure result
  where
    cleanup :: IORef Bool -> PostgresBeamTrans index db model event -> IO ()
    cleanup transactionCompleted pbt = do
        let OngoingTransaction connR localPool t0 = pbt ^. field @"transaction"
            conn = resource connR

            giveBackConnection :: IO ()
            giveBackConnection = do
                readIORef transactionCompleted >>= \case
                    True -> PG.commit conn
                    False -> PG.rollback conn
                putResource localPool conn
                t1 <- getCurrentTime
                pbt ^. field @"logger" $
                    DbTransactionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
        giveBackConnection `catchAll` \_ -> do
            t1 <- getCurrentTime
            pbt ^. field @"logger" $
                DbTransactionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
            destroyResource (beam ^. field @"connectionPool") localPool conn

    prepareTransaction
        :: Resource Connection
        -> LocalPool Connection
        -> IO (PostgresBeamTrans index db model event)
    prepareTransaction connR localPool = do
        t0 <- getCurrentTime
        PG.begin $ resource connR
        pure $
            PostgresBeamTrans
                { transaction = OngoingTransaction connR localPool t0
                , eventTableName = beam ^. field @"eventTableName"
                , projectionSpec = beam ^. field @"projectionSpec"
                , chunkSize = beam ^. field @"chunkSize"
                , logger = beam ^. field @"logger"
                }

withBeamStreamReadTransaction
    :: forall a index db model event
     . HasCallStack
    => PostgresBeam index db model event
    -> (Connection -> Stream IO a)
    -> Stream IO a
withBeamStreamReadTransaction beam stream =
    Stream.bracketIO startTrans rollbackTrans (\(_, conn) -> stream conn)
  where
    startTrans :: IO (OngoingTransaction, Connection)
    startTrans = do
        (connR, localPool) <- takeResource (beam ^. field @"connectionPool")
        t0 <- getCurrentTime
        PG.begin $ resource connR
        pure (OngoingTransaction connR localPool t0, resource connR)

    rollbackTrans :: (OngoingTransaction, Connection) -> IO ()
    rollbackTrans (trans, conn) = do
        let OngoingTransaction _ localPool t0 = trans

            giveBackConn :: IO ()
            giveBackConn = do
                PG.rollback conn
                putResource localPool conn
                t1 <- getCurrentTime
                beam ^. field @"logger" $
                    DbTransactionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
        giveBackConn `catchAll` \_ -> do
            t1 <- getCurrentTime
            beam ^. field @"logger" $
                DbTransactionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
            destroyResource (beam ^. field @"connectionPool") localPool conn

withScopedLock
    :: forall index db model event a
     . ( HasCallStack
       , IsPgIndex index
       )
    => PostgresBeamTrans index db model event
    -> index
    -> IO a
    -> IO a
withScopedLock pbt index action = do
    void $
        ( query
            (transactionConnection pbt)
            "select pg_advisory_xact_lock(?, ?)"
            ( tableHash :: Int32
            , indexHash :: Int32
            )
            :: IO [Only ()]
        )
    t0 <- getCurrentTime
    result <- action
    t1 <- getCurrentTime
    pbt ^. field @"logger" $
        EventTableLockDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
    pure result
  where
    tableHash = fromIntegral . hash $ pbt ^. field @"eventTableName"
    indexHash = fromIntegral . hash $ index

lockEventTableExclusive :: Connection -> EventTableName -> IO ()
lockEventTableExclusive conn eventTableName =
    void $
        execute_
            conn
            ("lock table " <> quoteIdent eventTableName <> " in access exclusive mode")

fromIndexedEventRow
    :: forall index event m
     . ( FromJSON event
       , IsPgIndex index
       , MonadThrow m
       )
    => IndexedEventRowOut
    -> m (index, Stored event, EventNumber)
fromIndexedEventRow (IndexedEventRowOut key streamIndex commitNumber rowTimestamp eventValue) =
    case fromJSON eventValue of
        Success event ->
            pure
                ( fromPgIndex streamIndex
                , Stored event rowTimestamp key
                , commitNumber
                )
        Error err ->
            throwM . EncodingError $
                "Failed to parse event "
                    <> show key
                    <> ": "
                    <> err
                    <> "\nWhen trying to parse:\n"
                    <> show eventValue

mkAllEventStream
    :: forall index event
     . ( FromJSON event
       , IsPgIndex index
       )
    => ChunkSize
    -> Connection
    -> EventTableName
    -> Stream IO (index, Stored event, EventNumber)
mkAllEventStream chunkSize conn eventTableName = do
    let step :: Cursor.Cursor -> IO (Maybe ([IndexedEventRowOut], Cursor.Cursor))
        step cursor = do
            rows <-
                Cursor.foldForward
                    cursor
                    chunkSize
                    (\acc row -> pure $ row : acc)
                    []
            case rows of
                Left [] -> pure Nothing
                Left xs -> pure $ Just (reverse xs, cursor)
                Right xs -> pure $ Just (reverse xs, cursor)

        queryText :: Query
        queryText =
            "select id, index, event_number, timestamp, event from "
                <> quoteIdent eventTableName
                <> " order by event_number"

    Stream.bracketIO
        (Cursor.declareCursor conn queryText)
        Cursor.closeCursor
        ( Stream.mapM fromIndexedEventRow
            . Stream.unfoldMany Unfold.fromList
            . Stream.unfoldrM step
        )

instance (Database Postgres db, IsPgIndex index, FromJSON event) => ReadModel (PostgresBeam index db model event) where
    type Model (PostgresBeam index db model event) = model
    type Event (PostgresBeam index db model event) = event
    type Index (PostgresBeam index db model event) = index

    applyEvent beam = applyModelEvent (beam ^. field @"projectionSpec")

    getModel :: MonadIO m => HasCallStack => PostgresBeam index db model event -> index -> m model
    getModel beam index =
        liftIO $ withBeamTrans beam (`loadModel` index)

    getEventList beam index =
        withResource (beam ^. field @"connectionPool") $ \conn ->
            fmap fst <$> queryEvents (resource conn) (beam ^. field @"eventTableName") index

    getEventStream beam index =
        withBeamStreamReadTransaction beam $
            \conn ->
                fst
                    <$> mkEventStream
                        (beam ^. field @"chunkSize")
                        conn
                        (mkEventQuery (beam ^. field @"eventTableName") index)

instance
    ( Database Postgres db
    , IsPgIndex index
    , FromJSON event
    , ToJSON event
    ) =>
    WriteModel (PostgresBeam index db model event)
    where
    postUpdateHook beam index model storedEvents =
        liftIO $ updateHook beam beam index model storedEvents

    transactionalUpdate beam index cmd = withRunInIO $ \runInIO ->
        withBeamTrans beam $ \pbt -> withScopedLock pbt index $ do
            model <- loadModel pbt index
            (returnFun, events) <- runInIO $ cmd model
            storedEvents <- traverse toStored events
            writeAndProjectStoredEvents pbt index storedEvents
            newModel <- loadModel pbt index
            pure (newModel, storedEvents, returnFun)
