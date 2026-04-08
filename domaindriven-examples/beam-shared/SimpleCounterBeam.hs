{-# LANGUAGE OverloadedRecordDot #-}

module SimpleCounterBeam
    ( CounterDb
    , CounterDomain
    , CounterEvent (..)
    , CounterModel (..)
    , StoredEvent (..)
    , applyEvent
    , counterDbSettings
    , createCounterBackend
    , loadCounterModelQuery
    , loadCounterValueQuery
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Control.Monad (void)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Pool.Introspection (Pool)
import Database.Beam
import Database.Beam.Migrate
    ( CheckedDatabaseSettings
    , checkedFieldNamed
    , checkedTableModification
    , defaultMigratableDbSettings
    , modifyCheckedTable
    , renameCheckedEntity
    , unCheckDatabase
    )
import Database.Beam.Postgres
import Database.PostgreSQL.Simple (execute_)
import DomainDriven
import DomainDriven.Persistance.Postgres
import DomainDriven.Persistance.Postgres.Types (quoteIdent)
import Prelude

data CounterModel = CounterModel
    { counter :: Int
    , previousCounter :: Int
    }
    deriving stock (Show, Eq, Generic)

data CounterEvent
    = CounterIncreased
    | CounterDecreased
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data StoredEvent = StoredEvent
    { event :: CounterEvent
    , timestamp :: UTCTime
    , uuid :: UUID
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

type CounterDomain = Domain CounterModel CounterEvent NoIndex

applyEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyEvent (CounterModel i _) (Stored ev _timestamp _uuid) = case ev of
    CounterIncreased -> CounterModel (i + 1) i
    CounterDecreased -> CounterModel (i - 1) i

data CounterProjectionT f = CounterProjection
    { counterId :: Columnar f Text
    , counter :: Columnar f Int32
    , previousCounter :: Columnar f Int32
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

instance Table CounterProjectionT where
    data PrimaryKey CounterProjectionT f = CounterProjectionId (Columnar f Text)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = CounterProjectionId . counterId

type CounterProjection = CounterProjectionT Identity

data CounterDb f = CounterDb
    { counterProjection :: f (TableEntity CounterProjectionT)
    }
    deriving stock (Generic)
    deriving anyclass (Database be)

counterRowKey :: Text
counterRowKey = "counter"

checkedCounterDb :: Text -> CheckedDatabaseSettings Postgres CounterDb
checkedCounterDb projectionTableName =
    defaultMigratableDbSettings
        `withDbModification` dbModification
            { counterProjection =
                renameCheckedEntity (const projectionTableName)
                    <> modifyCheckedTable
                        id
                        checkedTableModification
                            { counterId = checkedFieldNamed "counter_id"
                            , previousCounter = checkedFieldNamed "previous_counter"
                            }
            }

counterDbSettings :: Text -> DatabaseSettings Postgres CounterDb
counterDbSettings = unCheckDatabase . checkedCounterDb

createCounterBackend eventTableBaseName projectionTableName pool =
    postgresBeamBackend
        pool
        (InitialVersion eventTableBaseName)
        BeamProjectionSpec
            { checkedDb = checkedCounterDb projectionTableName
            , dropProjection = dropCounterProjection projectionTableName
            , applyModelEvent = applyEvent
            , loadProjectionModel = \db _ -> loadCounterModelQuery db
            , projectStoredEvent = \db _ -> projectCounterEvent db
            }
createCounterBackend
    :: EventTableBaseName
    -> Text
    -> Pool Connection
    -> IO (PostgresBeam NoIndex CounterDb CounterModel CounterEvent)

dropCounterProjection :: Text -> Connection -> IO ()
dropCounterProjection projectionTableName conn =
    void $
        execute_
            conn
            ("drop table if exists " <> quoteIdent (T.unpack projectionTableName))

loadCounterModelQuery :: DatabaseSettings Postgres CounterDb -> Pg CounterModel
loadCounterModelQuery db =
    fromMaybe (CounterModel 0 0) . fmap counterModelFromRow
        <$> loadCounterRow db

loadCounterValueQuery :: DatabaseSettings Postgres CounterDb -> Pg Int
loadCounterValueQuery db = do
    model <- loadCounterModelQuery db
    pure model.counter

loadCounterRow :: DatabaseSettings Postgres CounterDb -> Pg (Maybe CounterProjection)
loadCounterRow db =
    runSelectReturningOne $
        select $ do
            row <- all_ (counterProjection db)
            guard_ (row.counterId ==. val_ counterRowKey)
            pure row

counterModelFromRow :: CounterProjection -> CounterModel
counterModelFromRow row =
    CounterModel
        { counter = fromIntegral row.counter
        , previousCounter = fromIntegral row.previousCounter
        }

counterRowFromModel :: CounterModel -> CounterProjection
counterRowFromModel model =
    CounterProjection
        { counterId = counterRowKey
        , counter = fromIntegral model.counter
        , previousCounter = fromIntegral model.previousCounter
        }

projectCounterEvent
    :: DatabaseSettings Postgres CounterDb
    -> Stored CounterEvent
    -> Pg ()
projectCounterEvent db storedEvent = do
    currentRow <- loadCounterRow db
    let nextModel =
            applyEvent
                (fromMaybe (CounterModel 0 0) $ fmap counterModelFromRow currentRow)
                storedEvent
        nextRow = counterRowFromModel nextModel
    case currentRow of
        Nothing ->
            runInsert $
                insert
                    (counterProjection db)
                    (insertValues [nextRow])
        Just _ ->
            runUpdate $
                save
                    (counterProjection db)
                    nextRow
