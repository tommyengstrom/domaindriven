{-# LANGUAGE OverloadedRecordDot #-}

module PostgresCounterBeam
    ( CounterDb
    , CounterDomain
    , CounterEvent (..)
    , applyEvent
    , counterDbSettings
    , createBackend
    , loadCounterQuery
    )
where

import Control.Monad (void)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Pool.Introspection (Pool)
import Data.Text (Text)
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
import Event.V2 (CounterEvent (..))
import EventMigration (eventTable)
import Prelude

type CounterModel = Int
type CounterDomain = Domain CounterModel CounterEvent NoIndex

applyEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyEvent i (Stored ev _timestamp _uuid) = case ev of
    CounterIncreasedBy n -> i + n
    CounterDecreasedBy n -> i - n

data CounterProjectionT f = CounterProjection
    { counterId :: Columnar f Text
    , counter :: Columnar f Int32
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

instance Table CounterProjectionT where
    data PrimaryKey CounterProjectionT f = CounterProjectionId (Columnar f Text)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = CounterProjectionId . counterId

data CounterDb f = CounterDb
    { counterProjection :: f (TableEntity CounterProjectionT)
    }
    deriving stock (Generic)
    deriving anyclass (Database be)

counterRowKey :: Text
counterRowKey = "counter"

checkedCounterDb :: CheckedDatabaseSettings Postgres CounterDb
checkedCounterDb =
    defaultMigratableDbSettings
        `withDbModification` dbModification
            { counterProjection =
                renameCheckedEntity (const "postgres_counter_projection")
                    <> modifyCheckedTable
                        id
                        checkedTableModification
                            { counterId = checkedFieldNamed "counter_id"
                            }
            }

counterDbSettings :: DatabaseSettings Postgres CounterDb
counterDbSettings = unCheckDatabase checkedCounterDb

createBackend pool =
    postgresBeamBackend
        pool
        eventTable
        BeamProjectionSpec
            { checkedDb = checkedCounterDb
            , dropProjection = dropCounterProjection
            , applyModelEvent = applyEvent
            , loadProjectionModel = \db _ -> loadCounterQuery db
            , projectStoredEvent = \db _ -> projectCounterEvent db
            }
createBackend
    :: Pool Connection
    -> IO (PostgresBeam NoIndex CounterDb CounterModel CounterEvent)

dropCounterProjection :: Connection -> IO ()
dropCounterProjection conn =
    void $
        execute_
            conn
            ("drop table if exists " <> quoteIdent "postgres_counter_projection")

loadCounterQuery :: DatabaseSettings Postgres CounterDb -> Pg Int
loadCounterQuery db =
    fromMaybe 0 . fmap (fromIntegral . (.counter))
        <$> runSelectReturningOne
            ( select $ do
                row <- all_ (counterProjection db)
                guard_ (row.counterId ==. val_ counterRowKey)
                pure row
            )

projectCounterEvent :: DatabaseSettings Postgres CounterDb -> Stored CounterEvent -> Pg ()
projectCounterEvent db storedEvent = do
    current <- loadCounterQuery db
    currentRow <-
        runSelectReturningOne $
            select $ do
                row <- all_ (counterProjection db)
                guard_ (row.counterId ==. val_ counterRowKey)
                pure row
    let nextValue = applyEvent current storedEvent
        nextRow =
            CounterProjection
                { counterId = counterRowKey
                , counter = fromIntegral nextValue
                }
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
