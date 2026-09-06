module DomainDriven.Persistance.Snapshot
    ( SnapshotCodec
    , SnapshotFormat (..)
    , SnapshotFailure (..)
    , customSnapshotCodec
    , aesonJsonSnapshotCodec
    , serialiseCborSnapshotCodec
    , encodeSnapshot
    , decodeSnapshot
    , snapshotCodecFormat
    , snapshotCodecId
    , SnapshotProjectionName
    , mkSnapshotProjectionName
    , snapshotProjectionNameText
    , SnapshotProjectionRevision
    , mkSnapshotProjectionRevision
    , snapshotProjectionRevisionText
    , SnapshotCodecId
    , mkSnapshotCodecId
    , snapshotCodecIdText
    , SnapshotFrequency
    , mkEveryNEvents
    , everyNEvents
    , SnapshotTimeout
    , mkSnapshotTimeout
    , defaultSnapshotTimeout
    , snapshotTimeoutDuration
    , SnapshotQueueCapacity
    , mkSnapshotQueueCapacity
    , defaultSnapshotQueueCapacity
    , maximumPendingSnapshots
    , SnapshotFlushResult (..)
    , SnapshotConfig (..)
    , SnapshotStore (..)
    , SnapshotKey (..)
    , SnapshotCheckpoint (..)
    , StoredSnapshot (..)
    , SnapshotWriteResult (..)
    , postgresSnapshotStore
    )
where

import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise (Serialise)
import Codec.Serialise qualified as Serialise
import Control.DeepSeq (NFData, force)
import Control.Exception
    ( SomeAsyncException
    , SomeException
    , displayException
    , evaluate
    , fromException
    , tryJust
    )
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Pool.Introspection (Pool, resource, withResource)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (NominalDiffTime, UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
    ( Connection
    , Only (..)
    , Query
    , execute
    , execute_
    , query
    , withTransaction
    )
import Database.PostgreSQL.Simple.Types (Binary (..), (:.)(..))
import GHC.Generics (Generic)
import Prelude

-- | Selects the single populated payload column in the PostgreSQL store.
data SnapshotFormat = SnapshotJson | SnapshotCbor
    deriving (Show, Eq, Generic)

newtype SnapshotFailure = SnapshotFailure {snapshotFailureMessage :: String}
    deriving (Show, Eq, Generic)

newtype SnapshotProjectionName = SnapshotProjectionName {snapshotProjectionNameText :: Text}
    deriving (Show, Eq, Generic)

mkSnapshotProjectionName :: Text -> Maybe SnapshotProjectionName
mkSnapshotProjectionName name
    | validSnapshotIdentity name = Just (SnapshotProjectionName name)
    | otherwise = Nothing

-- | Change this revision when the seed, fold, or model representation changes.
newtype SnapshotProjectionRevision = SnapshotProjectionRevision {snapshotProjectionRevisionText :: Text}
    deriving (Show, Eq, Generic)

mkSnapshotProjectionRevision :: Text -> Maybe SnapshotProjectionRevision
mkSnapshotProjectionRevision revision
    | validSnapshotIdentity revision = Just (SnapshotProjectionRevision revision)
    | otherwise = Nothing

-- | A stable identity for one compatible encoding, including its revision.
newtype SnapshotCodecId = SnapshotCodecId {snapshotCodecIdText :: Text}
    deriving (Show, Eq, Generic)

mkSnapshotCodecId :: Text -> Maybe SnapshotCodecId
mkSnapshotCodecId codecId
    | validSnapshotIdentity codecId = Just (SnapshotCodecId codecId)
    | otherwise = Nothing

validSnapshotIdentity :: Text -> Bool
validSnapshotIdentity value = not (Text.null value) && not (Text.any (== '\0') value)

-- | A strict-byte codec. Use one of the supplied codecs or 'customSnapshotCodec'.
-- Decoded models are fully forced and synchronous codec exceptions are returned
-- as 'SnapshotFailure'; asynchronous cancellation is never swallowed.
data SnapshotCodec model = SnapshotCodec
    { codecId :: !SnapshotCodecId
    , codecFormat :: !SnapshotFormat
    , codecEncode :: model -> IO ByteString
    , codecDecode :: ByteString -> IO model
    }

-- | Build a strict-byte codec with an explicit identity and storage format.
customSnapshotCodec
    :: NFData model
    => SnapshotCodecId
    -> SnapshotFormat
    -> (model -> ByteString)
    -> (ByteString -> Either String model)
    -> SnapshotCodec model
customSnapshotCodec identity format encoder decoder =
    SnapshotCodec
        { codecId = identity
        , codecFormat = format
        , codecEncode = evaluate . force . encoder
        , codecDecode = \bytes -> case decoder bytes of
            Left failure -> fail failure
            Right model -> evaluate (force model)
        }

aesonJsonSnapshotCodec
    :: (ToJSON model, FromJSON model, NFData model)
    => SnapshotCodec model
aesonJsonSnapshotCodec =
    customSnapshotCodec (SnapshotCodecId "aeson-json-v1") SnapshotJson (LBS.toStrict . encode) eitherDecodeStrict'

serialiseCborSnapshotCodec
    :: (Serialise model, NFData model)
    => SnapshotCodec model
serialiseCborSnapshotCodec =
    customSnapshotCodec (SnapshotCodecId "serialise-cbor-v1") SnapshotCbor encodeCbor decodeCbor
  where
    encodeCbor :: Serialise model => model -> ByteString
    encodeCbor = LBS.toStrict . Serialise.serialise

    decodeCbor :: Serialise model => ByteString -> Either String model
    decodeCbor bytes = case CBOR.deserialiseFromBytes Serialise.decode (LBS.fromStrict bytes) of
        Left failure -> Left (show failure)
        Right (remaining, model)
            | LBS.null remaining -> Right model
            | otherwise -> Left "Trailing bytes after CBOR snapshot"

encodeSnapshot :: SnapshotCodec model -> model -> IO (Either SnapshotFailure ByteString)
encodeSnapshot codec = captureSnapshotFailure . codecEncode codec

decodeSnapshot :: SnapshotCodec model -> ByteString -> IO (Either SnapshotFailure model)
decodeSnapshot codec = captureSnapshotFailure . codecDecode codec

snapshotCodecFormat :: SnapshotCodec model -> SnapshotFormat
snapshotCodecFormat = codecFormat

snapshotCodecId :: SnapshotCodec model -> SnapshotCodecId
snapshotCodecId = codecId

captureSnapshotFailure :: IO a -> IO (Either SnapshotFailure a)
captureSnapshotFailure action = do
    result <- tryJust synchronous action
    pure $ case result of
        Left failure -> Left . SnapshotFailure $ displayException failure
        Right value -> Right value
  where
    synchronous :: SomeException -> Maybe SomeException
    synchronous failure = case fromException failure :: Maybe SomeAsyncException of
        Just _ -> Nothing
        Nothing -> Just failure

-- | A strictly positive number of relevant, per-index events between snapshots.
newtype SnapshotFrequency = SnapshotFrequency {everyNEvents :: Int64}
    deriving (Show, Eq, Generic)

mkEveryNEvents :: Int64 -> Maybe SnapshotFrequency
mkEveryNEvents n
    | n > 0 = Just (SnapshotFrequency n)
    | otherwise = Nothing

-- | A strictly positive bound for each runtime load/decode, delete, or
-- encode/store operation, representable as an 'Int' number of microseconds.
newtype SnapshotTimeout = SnapshotTimeout {snapshotTimeoutDuration :: NominalDiffTime}
    deriving (Show, Eq, Generic)

mkSnapshotTimeout :: NominalDiffTime -> Maybe SnapshotTimeout
mkSnapshotTimeout duration
    | duration > 0 && microseconds <= toInteger (maxBound :: Int) = Just (SnapshotTimeout duration)
    | otherwise = Nothing
  where
    microseconds :: Integer
    microseconds = ceiling (duration * 1000000)

defaultSnapshotTimeout :: SnapshotTimeout
defaultSnapshotTimeout = SnapshotTimeout 5

-- | Maximum distinct pending indexes, excluding the active write. An active
-- index's queued successor counts toward this limit. Must be strictly positive.
newtype SnapshotQueueCapacity = SnapshotQueueCapacity {maximumPendingSnapshots :: Int}
    deriving (Show, Eq, Generic)

mkSnapshotQueueCapacity :: Int -> Maybe SnapshotQueueCapacity
mkSnapshotQueueCapacity capacity
    | capacity > 0 = Just (SnapshotQueueCapacity capacity)
    | otherwise = Nothing

defaultSnapshotQueueCapacity :: SnapshotQueueCapacity
defaultSnapshotQueueCapacity = SnapshotQueueCapacity 64

-- | Completion means all queued attempts finished, not that all succeeded.
data SnapshotFlushResult = SnapshotFlushCompleted | SnapshotFlushTimedOut | SnapshotFlushClosed
    deriving (Show, Eq, Generic)

-- | Configure a fresh backend for each projection identity. Reusing a cache
-- across identity or fold changes can reuse an incompatible in-memory model.
data SnapshotConfig model = SnapshotConfig
    { snapshotProjectionName :: !SnapshotProjectionName
    , snapshotProjectionRevision :: !SnapshotProjectionRevision
    , snapshotCodec :: !(SnapshotCodec model)
    , snapshotFrequency :: !SnapshotFrequency
    , snapshotTimeout :: !SnapshotTimeout
    , snapshotQueueCapacity :: !SnapshotQueueCapacity
    , snapshotStore :: !SnapshotStore
    }

data SnapshotKey = SnapshotKey
    { snapshotEventTableName :: !String
    , snapshotEventIndex :: !Text
    , snapshotKeyProjectionName :: !SnapshotProjectionName
    , snapshotKeyProjectionRevision :: !SnapshotProjectionRevision
    , snapshotKeyCodecId :: !SnapshotCodecId
    }
    deriving (Show, Eq, Generic)

data SnapshotCheckpoint = SnapshotCheckpoint
    { snapshotEventNumber :: !Int64
    , snapshotEventId :: !UUID
    , snapshotEventCount :: !Int64
    -- ^ Relevant events through this checkpoint for its index, independent of
    -- the global event number.
    }
    deriving (Show, Eq, Generic)

data StoredSnapshot = StoredSnapshot
    { storedSnapshotCheckpoint :: !SnapshotCheckpoint
    , storedSnapshotTimestamp :: !UTCTime
    , storedSnapshotFormat :: !SnapshotFormat
    , storedSnapshotPayload :: !ByteString
    }
    deriving (Show, Eq, Generic)

data SnapshotWriteResult = SnapshotStored | NewerSnapshotRetained
    deriving (Show, Eq, Generic)

-- | Storage seam for disposable, latest-only snapshots.
--
-- Implementations must load by exact key, retain a newer checkpoint on store,
-- permit equal-checkpoint replacement, and delete only the exact loaded row
-- (including checkpoint, timestamp, format, and payload). A JSON codec must
-- accept equivalent JSON encodings: PostgreSQL normalizes JSON in @jsonb@.
data SnapshotStore = SnapshotStore
    { initializeSnapshotStore :: IO ()
    , loadSnapshot :: SnapshotKey -> IO (Maybe StoredSnapshot)
    , storeSnapshot :: SnapshotKey -> SnapshotCheckpoint -> SnapshotFormat -> ByteString -> IO SnapshotWriteResult
    , deleteSnapshot :: SnapshotKey -> StoredSnapshot -> IO ()
    }

postgresSnapshotStore :: Pool Connection -> SnapshotStore
postgresSnapshotStore pool =
    SnapshotStore
        { initializeSnapshotStore = withResource pool (initialize . resource)
        , loadSnapshot = \key -> withResource pool $ \conn -> load (resource conn) key
        , storeSnapshot = \key checkpoint format payload -> withResource pool $ \conn -> store (resource conn) key checkpoint format payload
        , deleteSnapshot = \key snapshot -> withResource pool $ \conn -> delete (resource conn) key snapshot
        }
  where
    initialize :: Connection -> IO ()
    initialize conn = withTransaction conn $ do
        void (query conn "select pg_advisory_xact_lock(?)" (Only snapshotDdlLock) :: IO [Only ()])
        void $ execute_ conn createSnapshotTable

    load :: Connection -> SnapshotKey -> IO (Maybe StoredSnapshot)
    load conn key = do
        rows <- query conn ("select event_number, event_id, event_count, timestamp, snapshot_json::text, snapshot_cbor from " <> snapshotTable <> " where " <> snapshotKeyPredicate) (snapshotKeyParameters key)
            :: IO [(Int64, UUID, Int64, UTCTime, Maybe ByteString, Maybe ByteString)]
        pure $ case rows of
            [(number, eventId, eventCount, timestamp, Just json, Nothing)] -> Just $ StoredSnapshot (SnapshotCheckpoint number eventId eventCount) timestamp SnapshotJson json
            [(number, eventId, eventCount, timestamp, Nothing, Just cbor)] -> Just $ StoredSnapshot (SnapshotCheckpoint number eventId eventCount) timestamp SnapshotCbor cbor
            _ -> Nothing

    store :: Connection -> SnapshotKey -> SnapshotCheckpoint -> SnapshotFormat -> ByteString -> IO SnapshotWriteResult
    store conn key SnapshotCheckpoint{snapshotEventNumber, snapshotEventId, snapshotEventCount} format payload = do
        stored <- query conn upsertSnapshot (snapshotKeyParameters key :. (snapshotEventNumber, snapshotEventId, snapshotEventCount) :. snapshotPayloadParameters format payload)
            :: IO [Only Int]
        case stored of
            [Only _] -> pure SnapshotStored
            [] -> pure NewerSnapshotRetained
            _ -> fail "Unexpected snapshot upsert result"

    delete :: Connection -> SnapshotKey -> StoredSnapshot -> IO ()
    delete conn key StoredSnapshot{storedSnapshotCheckpoint = SnapshotCheckpoint{snapshotEventNumber, snapshotEventId, snapshotEventCount}, storedSnapshotTimestamp, storedSnapshotFormat, storedSnapshotPayload} =
        void $ execute conn
            ("delete from " <> snapshotTable <> " where " <> snapshotKeyPredicate <> " and event_number = ? and event_id = ? and event_count = ? and timestamp = ? and snapshot_json is not distinct from ?::jsonb and snapshot_cbor is not distinct from ?")
            (snapshotKeyParameters key :. (snapshotEventNumber, snapshotEventId, snapshotEventCount, storedSnapshotTimestamp) :. snapshotPayloadParameters storedSnapshotFormat storedSnapshotPayload)

snapshotKeyParameters :: SnapshotKey -> (String, Text, Text, Text, Text)
snapshotKeyParameters SnapshotKey{snapshotEventTableName, snapshotEventIndex, snapshotKeyProjectionName, snapshotKeyProjectionRevision, snapshotKeyCodecId} =
    ( snapshotEventTableName
    , snapshotEventIndex
    , snapshotProjectionNameText snapshotKeyProjectionName
    , snapshotProjectionRevisionText snapshotKeyProjectionRevision
    , snapshotCodecIdText snapshotKeyCodecId
    )

snapshotPayloadParameters :: SnapshotFormat -> ByteString -> (Maybe Text, Maybe (Binary ByteString))
snapshotPayloadParameters format payload = case format of
    SnapshotJson -> (Just $ Text.decodeUtf8 payload, Nothing)
    SnapshotCbor -> (Nothing, Just $ Binary payload)

snapshotTable :: Query
snapshotTable = "public.\"domaindriven-snapshots\""

snapshotKeyPredicate :: Query
snapshotKeyPredicate = "event_table_name = ? and event_index = ? and projection_name = ? and projection_revision = ? and codec_id = ?"

snapshotDdlLock :: Int64
snapshotDdlLock = 5764758386540796211

createSnapshotTable :: Query
createSnapshotTable = "CREATE TABLE IF NOT EXISTS " <> snapshotTable <> " (event_table_name text NOT NULL CHECK (event_table_name <> ''), event_index text NOT NULL, projection_name text NOT NULL CHECK (projection_name <> ''), projection_revision text NOT NULL CHECK (projection_revision <> ''), codec_id text NOT NULL CHECK (codec_id <> ''), event_number bigint NOT NULL CHECK (event_number > 0), event_id uuid NOT NULL, event_count bigint NOT NULL CHECK (event_count > 0), timestamp timestamptz NOT NULL DEFAULT now(), snapshot_json jsonb, snapshot_cbor bytea, PRIMARY KEY (event_table_name, event_index, projection_name, projection_revision, codec_id), CHECK (num_nonnulls(snapshot_json, snapshot_cbor) = 1));"

upsertSnapshot :: Query
upsertSnapshot = "insert into " <> snapshotTable <> " as current (event_table_name, event_index, projection_name, projection_revision, codec_id, event_number, event_id, event_count, snapshot_json, snapshot_cbor) values (?, ?, ?, ?, ?, ?, ?, ?, ?::jsonb, ?) on conflict (event_table_name, event_index, projection_name, projection_revision, codec_id) do update set event_number = excluded.event_number, event_id = excluded.event_id, event_count = excluded.event_count, timestamp = now(), snapshot_json = excluded.snapshot_json, snapshot_cbor = excluded.snapshot_cbor where current.event_number <= excluded.event_number returning 1"
