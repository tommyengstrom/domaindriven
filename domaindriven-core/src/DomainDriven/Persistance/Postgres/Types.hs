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
import Data.List (intercalate)
import Data.Pool.Introspection as Pool
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
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
type MigrationTag = String
type EventTableName = String
type PreviousEventTableName = String
type ChunkSize = Int
type ParseConcurrency = Int

class Hashable a => IsPgIndex a where
    toPgIndex :: a -> Text -- FIXME: Should not be Text
    fromPgIndex :: Text -> a
    toQuery :: a -> PG.Query
    toQuery t = "'" <> (fromString . T.unpack . toPgIndex) t <> "'"

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
-- locks on the previous table; ending it would release those locks mid-chain and
-- break the atomicity of the migration.
type EventMigration = PreviousEventTableName -> EventTableName -> Connection -> IO ()

-- | The chain of event table versions this code knows about, newest first.
--
-- @
-- eventTable :: EventTable
-- eventTable =
--     MigrateWith "split-name" migrationV50
--         $ MigrateWith "add-email" migrationV49
--         $ TableName "events" 48
-- -- current table: events_v50
-- @
--
-- The current table is @\<base\>_v\<baseVersion + number of MigrateWith wrappers\>@. Every
-- applied migration is recorded, by tag, in the @domaindriven_migrations@ table, and
-- 'DomainDriven.Persistance.Postgres.Internal.postgresWriteModel' verifies at startup
-- that the tags in code agree with the ones recorded in the database.
--
-- To delete old migrations from code, remove their 'MigrateWith' wrappers and bump the
-- 'TableName' version accordingly (only once every database has been migrated past
-- them). Renaming a tag requires updating the recorded tag as well
-- (@update domaindriven_migrations set tag = ... where base_name = ... and version = ...@).
data EventTable
    = -- | A migration to the next version. The tag identifies the migration in the
      -- @domaindriven_migrations@ table; it must be non-empty and unique within the chain.
      MigrateWith MigrationTag EventMigration EventTable
    | -- | The oldest version this code still knows about (not necessarily 1). The base
      -- name must be non-empty and contain only @[a-zA-Z0-9_]@; the version must be @>= 1@.
      TableName EventTableBaseName EventTableVersion

-- | Why a row exists in the @domaindriven_migrations@ table.
data MigrationOrigin
    = -- | The migration function ran and the previous table was retired.
      OriginMigration
    | -- | The table was created on a database that had no tables for the base name.
      -- Rows for the whole chain are recorded but no migration function ran.
      OriginBootstrap
    | -- | The table already existed without a metadata row (a database from before
      -- 0.7.0, or a migration run by pre-0.7.0 code); the tag is unknown.
      OriginAdopted
    deriving stock (Show, Eq, Generic)

migrationOriginText :: MigrationOrigin -> String
migrationOriginText = \case
    OriginMigration -> "migration"
    OriginBootstrap -> "bootstrap"
    OriginAdopted -> "adopted"

parseMigrationOrigin :: String -> Maybe MigrationOrigin
parseMigrationOrigin = \case
    "migration" -> Just OriginMigration
    "bootstrap" -> Just OriginBootstrap
    "adopted" -> Just OriginAdopted
    _ -> Nothing

-- | A version at which the migration tag in code disagrees with the database.
data TagDisagreement = TagDisagreement
    { version :: EventTableVersion
    , codeTag :: MigrationTag
    -- ^ The tag the 'EventTable' chain has for this version.
    , recordedTag :: Maybe MigrationTag
    -- ^ The tag recorded for this version, if the version is recorded with a known tag.
    , recordedOrigin :: Maybe MigrationOrigin
    -- ^ The origin of the recorded row, if any.
    , codeTagRecordedAt :: Maybe EventTableVersion
    -- ^ The version at which the code's tag is recorded, when it is recorded elsewhere.
    }
    deriving stock (Show, Eq, Generic)

-- | Startup verification of the 'EventTable' chain against the database failed. Nothing
-- was migrated; the message from 'displayException' says what to do.
data MigrationError
    = -- | The base name is empty or contains characters outside @[a-zA-Z0-9_]@.
      InvalidEventTableBaseName EventTableBaseName
    | -- | The 'TableName' version is below 1.
      InvalidEventTableVersion EventTableBaseName EventTableVersion
    | -- | The tag of the migration producing this version is empty.
      InvalidMigrationTag EventTableBaseName EventTableVersion MigrationTag
    | -- | The tag is used for more than one version of the chain.
      DuplicateMigrationTag EventTableBaseName MigrationTag [EventTableVersion]
    | -- | The tags in code disagree with the tags recorded in the database. Carries the
      -- 'TableName' version of the chain, the disagreements, and, when the code's tag
      -- sequence matches the recorded history at another offset, that offset (positive:
      -- the tags are recorded that many versions later than the code says).
      MigrationTagMismatch EventTableBaseName EventTableVersion [TagDisagreement] (Maybe Int)
    | -- | The database's current version is higher than the code's current version.
      DatabaseAheadOfCode EventTableBaseName EventTableVersion EventTableVersion
    | -- | The database's current version is lower than the 'TableName' version, so the
      -- code cannot bring it forward. Carries the database version, the 'TableName'
      -- version and the number of events in the database's current table.
      DatabaseBehindCodeBase EventTableBaseName EventTableVersion EventTableVersion Int64
    | -- | The metadata records this version as current, but its table does not exist in
      -- the given schema.
      CurrentEventTableMissing EventTableBaseName EventTableVersion String
    | -- | An event table exists that is not recorded in the metadata and cannot be
      -- adopted, for the given reason.
      UnrecordedEventTable EventTableBaseName EventTableName String
    | -- | The metadata rows for the base name could not be interpreted.
      InvalidMigrationMetadata EventTableBaseName String
    deriving stock (Show, Eq, Generic)

instance Exception MigrationError where
    displayException = \case
        InvalidEventTableBaseName base ->
            prefix
                <> "Invalid event table base name "
                <> show base
                <> ". Base names must be non-empty and contain only [a-zA-Z0-9_]."
        InvalidEventTableVersion base v ->
            prefix
                <> "Invalid version in TableName "
                <> show base
                <> " "
                <> show v
                <> ". Event table versions start at 1."
        InvalidMigrationTag base v t ->
            prefix
                <> "Invalid migration tag "
                <> show t
                <> " for "
                <> tableName base v
                <> ". Migration tags must be non-empty."
        DuplicateMigrationTag base t vs ->
            prefix
                <> "Migration tag "
                <> show t
                <> " is used more than once in the "
                <> show base
                <> " chain (versions "
                <> commaSep (map show vs)
                <> "). Tags must be unique within a chain."
        MigrationTagMismatch base codeBase disagreements shift ->
            unlines' $
                [prefix <> "Migration tag mismatch for event table " <> show base <> ":"]
                    <> map (("  - " <>) . showDisagreement base) disagreements
                    <> [ case shift of
                            Just n ->
                                "The code's tag sequence matches the recorded history "
                                    <> show (abs n)
                                    <> " version(s) "
                                    <> (if n < 0 then "earlier" else "later")
                                    <> " - the TableName base version is probably "
                                    <> show (codeBase + n)
                                    <> ", not "
                                    <> show codeBase
                                    <> "."
                            Nothing ->
                                "The code's tag sequence does not match the recorded history at any offset."
                       , "No migration was run. Fix the EventTable chain, or, if the tags were \
                         \renamed on purpose, update the recorded tags in domaindriven_migrations, \
                         \then restart."
                       ]
        DatabaseAheadOfCode base dbVersion codeVersion ->
            prefix
                <> "The database is ahead of this code for event table "
                <> show base
                <> ": the current table is "
                <> tableName base dbVersion
                <> " but this code's chain ends at "
                <> tableName base codeVersion
                <> ". Deploy code that includes the migrations up to version "
                <> show dbVersion
                <> ". Refusing to start."
        DatabaseBehindCodeBase base dbVersion codeBase eventCount ->
            prefix
                <> "The database is behind the oldest version this code knows about for event table "
                <> show base
                <> ": the current table is "
                <> tableName base dbVersion
                <> " (holding "
                <> show eventCount
                <> " event(s)) but the chain starts at TableName "
                <> show base
                <> " "
                <> show codeBase
                <> ", so this code cannot bring it forward. Deploy code whose chain still \
                   \contains the migrations from version "
                <> show dbVersion
                <> " to "
                <> show codeBase
                <> ", let it migrate, then upgrade."
                <> ( if eventCount == 0
                        then
                            " The table holds no events, so if it was never used it can \
                            \instead be dropped together with its metadata rows (dropEventTables), \
                            \after which a restart bootstraps directly at the current version."
                        else ""
                   )
        CurrentEventTableMissing base v schema ->
            prefix
                <> "domaindriven_migrations records "
                <> tableName base v
                <> " as the current event table for "
                <> show base
                <> ", but no such table exists in schema "
                <> show schema
                <> ". If the event tables were dropped on purpose, delete the metadata as well \
                   \(`delete from domaindriven_migrations where base_name = '"
                <> base
                <> "'`, or use dropEventTables from DomainDriven.Persistance.Postgres) and \
                   \restart to bootstrap a fresh table. Otherwise restore the table before starting."
        UnrecordedEventTable base name reason ->
            prefix
                <> "Found event table "
                <> show name
                <> " for "
                <> show base
                <> " that is not recorded in domaindriven_migrations and cannot be adopted: "
                <> reason
                <> ". Rename or drop the table (or record it in domaindriven_migrations by hand) \
                   \and restart."
        InvalidMigrationMetadata base reason ->
            prefix
                <> "Cannot interpret the domaindriven_migrations rows for "
                <> show base
                <> ": "
                <> reason
      where
        prefix = "[DomainDriven] "
        tableName base v = base <> "_v" <> show v
        commaSep = intercalate ", "
        unlines' = intercalate "\n"
        showDisagreement base (TagDisagreement v code recorded origin elsewhere) =
            tableName base v
                <> ": code says "
                <> show code
                <> case (recorded, elsewhere) of
                    (Just r, _) ->
                        ", database recorded "
                            <> show r
                            <> originNote origin
                            <> maybe
                                ""
                                (\w -> "; " <> show code <> " was recorded at " <> tableName base w)
                                elsewhere
                    (Nothing, Just w) ->
                        ", but the database recorded " <> show code <> " at " <> tableName base w
                    (Nothing, Nothing) -> ", the database has no tag for this version"
        originNote = \case
            Just OriginBootstrap -> " (recorded at bootstrap; no migration ran)"
            Just OriginMigration -> " (produced by a migration run)"
            Just OriginAdopted -> " (adopted)"
            Nothing -> ""

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
