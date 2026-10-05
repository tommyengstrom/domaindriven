module DomainDriven.Persistance.Postgres.Types
    ( module DomainDriven.Persistance.Postgres.Types
    , Pool.PoolConfig
    , Pool.setNumStripes
    )
where

import Control.DeepSeq (NFData)
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Int
import Data.Pool.Introspection as Pool
import Data.String
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.FromField qualified as FF
import DomainDriven.Persistance.Class
import GHC.Generics (Generic)
import Prelude

-- | Quote a PostgreSQL identifier (table/column name).
-- Escapes embedded double quotes by doubling them per SQL standard.
quoteIdent :: String -> PG.Query
quoteIdent name = "\"" <> fromString (concatMap escChar name) <> "\""
  where
    escChar '"' = "\"\""
    escChar c = [c]

data PersistanceError
    = EncodingError String
    | ValueError String
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Exception, NFData)

type EventTableBaseName = String
type EventTableVersion = Int
type EventTableName = String
type PreviousEventTableName = String
type ChunkSize = Int
type ParseConcurrency = Int

class Hashable a => IsPgIndex a where
    toPgIndex :: a -> Text -- FIXME: Should not be Text
    fromPgIndex :: Text -> a

instance IsPgIndex NoIndex where
    toPgIndex = const "0"
    fromPgIndex _ = NoIndex

instance IsPgIndex Indexed where
    toPgIndex (Indexed t) = t
    fromPgIndex = Indexed

-- | One migration step: copy (and transform) the events of the previous event table
-- into the new one. The function is given the name of the previous table, the name of
-- the new table, and the connection on which the migration transaction runs.
--
-- Contract: the function must not commit, roll back, or otherwise end the transaction
-- of the connection it is given (no @commit@, @rollback@, @withTransaction@, ...). The
-- whole migration chain runs in the single startup transaction that also holds the
-- locks on the previous table; ending it releases those locks mid-chain and breaks the
-- atomicity of the migration. A function that ends it anyway makes startup fail with
-- 'MigrationEndedTransaction'.
type EventMigration = PreviousEventTableName -> EventTableName -> Connection -> IO ()

-- | The chain of event table versions this code knows about, newest first.
--
-- @
-- eventTable :: EventTable
-- eventTable =
--     MigrateTo 50 migrationV50
--         $ MigrateTo 49 migrationV49
--         $ TableName "events" 48
-- -- current table: events_v50
-- @
--
-- The tables in the first schema on the search path containing this base name are the
-- only state: the highest existing @\<base\>_v\<n\>@ is the current one, and
-- 'DomainDriven.Persistance.Postgres.Internal.postgresWriteModel'
-- runs the steps above it at startup. A database without any table for the base name
-- gets only the current table; no migration function runs on it.
-- Before accepting an existing current table, startup checks that the previous
-- existing table is retired; otherwise it throws 'IncompleteMigration'.
--
-- The numbers are the only link between the code and the database, so a chain numbered
-- one too high runs its newest migration again, on the live table. Before deploying a
-- renumbered chain, check that
-- 'DomainDriven.Persistance.Postgres.Internal.getEventTableName' gives the name of the
-- live table.
--
-- To delete old migrations from code, remove their 'MigrateTo' wrappers and raise the
-- 'TableName' version to one below the oldest remaining step (only once every database
-- has been migrated past them).
data EventTable
    = -- | A migration producing the given version. Steps must be numbered consecutively,
      -- starting one above the 'TableName' version.
      MigrateTo EventTableVersion EventMigration EventTable
    | -- | The oldest version this code still knows about (not necessarily 1). The base
      -- name must be non-empty and contain only @[a-zA-Z0-9_]@, the version must be
      -- @>= 1@, and the current table name must fit in 'maxEventTableNameLength'.
      TableName EventTableBaseName EventTableVersion

-- | PostgreSQL's identifier limit.
maxEventTableNameLength :: Int
maxEventTableNameLength = 63

-- | What every table of a base name starts with: @\<base\>_v@.
eventTablePrefix :: EventTableBaseName -> String
eventTablePrefix base = base <> "_v"

-- | Table name for a version of a base name: @\<base\>_v\<version\>@.
eventTableNameFor :: EventTableBaseName -> EventTableVersion -> EventTableName
eventTableNameFor base v = eventTablePrefix base <> show v

-- | Invalid table configuration or a migration failure. The message from
-- 'displayException' describes the failure.
data MigrationError
    = -- | The base name is empty or contains characters outside @[a-zA-Z0-9_]@.
      InvalidEventTableBaseName EventTableBaseName
    | -- | A raw table name is empty or contains characters outside @[a-zA-Z0-9_]@.
      InvalidEventTableName EventTableName
    | -- | The 'TableName' version is below 1.
      InvalidEventTableVersion EventTableBaseName EventTableVersion
    | -- | The table name is longer than 'maxEventTableNameLength'.
      EventTableNameTooLong EventTableName
    | -- | A 'MigrateTo' step is not numbered one above the step below it. Carries the
      -- expected and the actual version.
      MisnumberedMigration EventTableBaseName EventTableVersion EventTableVersion
    | -- | The database's version is higher than the code's current version. Carries the
      -- database version and the code version.
      DatabaseAheadOfCode EventTableBaseName EventTableVersion EventTableVersion
    | -- | The database's version is lower than the 'TableName' version, so the code
      -- cannot bring it forward. Carries the database version and the 'TableName'
      -- version.
      DatabaseBelowBaseVersion EventTableBaseName EventTableVersion EventTableVersion
    | -- | The previous existing table has no enabled retirement trigger, so the
      -- newest table cannot be accepted as a completed migration. Carries the
      -- previous and newest table names.
      IncompleteMigration PreviousEventTableName EventTableName
    | -- | The migration function producing this table committed or rolled back the
      -- startup transaction, against the 'EventMigration' contract.
      MigrationEndedTransaction EventTableName
    deriving stock (Show, Eq, Generic)

instance Exception MigrationError where
    displayException =
        ("[DomainDriven] " <>) . \case
            InvalidEventTableBaseName base ->
                "Invalid event table base name "
                    <> show base
                    <> ". Base names must be non-empty and contain only [a-zA-Z0-9_]."
            InvalidEventTableName name ->
                "Invalid event table name "
                    <> show name
                    <> ". Names must be non-empty and contain only [a-zA-Z0-9_]."
            InvalidEventTableVersion base v ->
                "Invalid version in TableName "
                    <> show base
                    <> " "
                    <> show v
                    <> ". Event table versions start at 1."
            EventTableNameTooLong name ->
                "Event table name "
                    <> show name
                    <> " is "
                    <> show (length name)
                    <> " characters long; the limit is "
                    <> show maxEventTableNameLength
                    <> "."
            MisnumberedMigration base expected actual ->
                "Misnumbered migration for event table "
                    <> show base
                    <> ": found MigrateTo "
                    <> show actual
                    <> " where MigrateTo "
                    <> show expected
                    <> " was expected. The chain is written newest first, and each step must \
                       \be numbered one above the step it wraps (the first one above the \
                       \TableName version)."
            DatabaseAheadOfCode base dbVersion codeVersion ->
                "The database is ahead of this code for event table "
                    <> show base
                    <> ": the current table is "
                    <> eventTableNameFor base dbVersion
                    <> " but this code's chain ends at "
                    <> eventTableNameFor base codeVersion
                    <> ". Deploy code that includes the migrations up to version "
                    <> show dbVersion
                    <> ". To roll the database back instead: stop every instance, run \
                       \`drop trigger retired on "
                    <> quotedName (eventTableNameFor base codeVersion)
                    <> "`, move any events to keep from the newer tables into "
                    <> quotedName (eventTableNameFor base codeVersion)
                    <> " by hand, then drop the newer tables."
            DatabaseBelowBaseVersion base dbVersion baseVersion ->
                "The database is below the oldest version this code knows about for event table "
                    <> show base
                    <> ": the current table is "
                    <> eventTableNameFor base dbVersion
                    <> " but the chain starts at TableName "
                    <> show base
                    <> " "
                    <> show baseVersion
                    <> ". Deploy code whose chain still contains the migrations from version "
                    <> show dbVersion
                    <> " to "
                    <> show baseVersion
                    <> ", let it migrate, then upgrade."
            IncompleteMigration prevName name ->
                "Cannot use event table "
                    <> quotedName name
                    <> " because the previous existing table "
                    <> quotedName prevName
                    <> " is not retired. Inspect both tables and repair the incomplete \
                       \migration before restarting."
            MigrationEndedTransaction name ->
                "The migration producing "
                    <> name
                    <> " committed or rolled back the startup transaction. Migration \
                       \functions must not end the transaction of the connection they \
                       \are given. The previous table is still live; if "
                    <> name
                    <> " exists, drop it before starting again."
      where
        -- Names are validated to [a-zA-Z0-9_], so quoting needs no escaping.
        quotedName :: EventTableName -> String
        quotedName name = "\"" <> name <> "\""

newtype EventNumber = EventNumber {unEventNumber :: Int64}
    deriving (Show, Generic)
    deriving newtype (Eq, Ord, Num, NFData)

instance FF.FromField EventNumber where
    fromField f bs = EventNumber <$> FF.fromField f bs

data NumberedModel m = NumberedModel
    { model :: !m
    , eventNumber :: !EventNumber
    }
    deriving (Show, Generic)

data NumberedEvent e = NumberedEvent
    { event :: !(Stored e)
    , eventNumber :: !EventNumber
    }
    deriving (Show, Generic)

data OngoingTransaction = OngoingTransaction
    { connectionResource :: Pool.Resource Connection
    , localPool :: Pool.LocalPool Connection
    , transactionStartTime :: UTCTime
    }
    deriving (Generic)

data EventRowOut = EventRowOut
    { key :: UUID
    , commitNumber :: EventNumber
    , timestamp :: UTCTime
    , event :: ByteString
    }
    deriving (Show, Eq, Generic, PG.FromRow)

fromEventRowResult
    :: FromJSON e => EventRowOut -> Either PersistanceError (Stored e, EventNumber)
fromEventRowResult (EventRowOut evKey no ts ev) = case eitherDecodeStrict' ev of
    Right a -> a `seq` Right (Stored a ts evKey, no)
    Left err ->
        Left
            . EncodingError
            $ "Failed to parse event "
                <> show evKey
                <> ": "
                <> err
                <> "\nWhen trying to parse:\n"
                <> show ev

fromEventRow :: (FromJSON e, MonadThrow m) => EventRowOut -> m (Stored e, EventNumber)
fromEventRow = either throwM pure . fromEventRowResult
