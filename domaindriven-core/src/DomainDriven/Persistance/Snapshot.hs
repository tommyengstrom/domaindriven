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
    , SnapshotFrequency
    , mkEveryNEvents
    , everyNEvents
    , SnapshotTimeout
    , mkSnapshotTimeout
    , defaultSnapshotTimeout
    , snapshotTimeoutDuration
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
import GHC.Generics (Generic)
import Prelude

-- | Selects the single populated payload column in the PostgreSQL store.
data SnapshotFormat = SnapshotJson | SnapshotCbor
    deriving (Show, Eq, Generic)

newtype SnapshotFailure = SnapshotFailure {snapshotFailureMessage :: String}
    deriving (Show, Eq, Generic)

-- | A strict-byte codec. Use one of the supplied codecs or 'customSnapshotCodec'.
-- Decoded models are fully forced and synchronous codec exceptions are returned
-- as 'SnapshotFailure'; asynchronous cancellation is never swallowed.
data SnapshotCodec model = SnapshotCodec
    { codecFormat :: !SnapshotFormat
    , codecEncode :: model -> IO ByteString
    , codecDecode :: ByteString -> IO model
    }

-- | Build a strict-byte codec with an explicit storage format.
customSnapshotCodec
    :: NFData model
    => SnapshotFormat
    -> (model -> ByteString)
    -> (ByteString -> Either String model)
    -> SnapshotCodec model
customSnapshotCodec format encoder decoder =
    SnapshotCodec
        { codecFormat = format
        , codecEncode = evaluate . force . encoder
        , codecDecode = \bytes -> case decoder bytes of
            Left failure -> fail failure
            Right model -> evaluate (force model)
        }

aesonJsonSnapshotCodec
    :: (ToJSON model, FromJSON model, NFData model)
    => SnapshotCodec model
aesonJsonSnapshotCodec =
    customSnapshotCodec SnapshotJson (LBS.toStrict . encode) eitherDecodeStrict'

serialiseCborSnapshotCodec
    :: (Serialise model, NFData model)
    => SnapshotCodec model
serialiseCborSnapshotCodec =
    customSnapshotCodec SnapshotCbor encodeCbor decodeCbor
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
-- encode/store operation.
newtype SnapshotTimeout = SnapshotTimeout {snapshotTimeoutDuration :: NominalDiffTime}
    deriving (Show, Eq, Generic)

mkSnapshotTimeout :: NominalDiffTime -> Maybe SnapshotTimeout
mkSnapshotTimeout duration
    | duration > 0 = Just (SnapshotTimeout duration)
    | otherwise = Nothing

defaultSnapshotTimeout :: SnapshotTimeout
defaultSnapshotTimeout = SnapshotTimeout 5

data SnapshotConfig model = SnapshotConfig
    { snapshotCodec :: !(SnapshotCodec model)
    , snapshotFrequency :: !SnapshotFrequency
    , snapshotTimeout :: !SnapshotTimeout
    , snapshotStore :: !SnapshotStore
    }

data SnapshotKey = SnapshotKey
    { snapshotEventTableName :: !String
    , snapshotEventIndex :: !Text
    }
    deriving (Show, Eq, Generic)

data SnapshotCheckpoint = SnapshotCheckpoint
    { snapshotEventNumber :: !Int64
    , snapshotEventId :: !UUID
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
-- (including checkpoint, timestamp, format, and payload).
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
    load conn SnapshotKey{snapshotEventTableName, snapshotEventIndex} = do
        rows <- query conn "select event_number, event_id, timestamp, snapshot_json::text, snapshot_cbor from public.\"domaindriven-snapshots\" where event_table_name = ? and event_index = ?" (snapshotEventTableName, snapshotEventIndex)
            :: IO [(Int64, UUID, UTCTime, Maybe ByteString, Maybe ByteString)]
        pure $ case rows of
            [(number, eventId, timestamp, Just json, Nothing)] -> Just $ StoredSnapshot (SnapshotCheckpoint number eventId) timestamp SnapshotJson json
            [(number, eventId, timestamp, Nothing, Just cbor)] -> Just $ StoredSnapshot (SnapshotCheckpoint number eventId) timestamp SnapshotCbor cbor
            _ -> Nothing

    store :: Connection -> SnapshotKey -> SnapshotCheckpoint -> SnapshotFormat -> ByteString -> IO SnapshotWriteResult
    store conn SnapshotKey{snapshotEventTableName, snapshotEventIndex} SnapshotCheckpoint{snapshotEventNumber, snapshotEventId} format payload = do
        stored <- case format of
            SnapshotJson -> query conn upsertJson (snapshotEventTableName, snapshotEventIndex, snapshotEventNumber, snapshotEventId, Text.decodeUtf8 payload) :: IO [Only Int]
            SnapshotCbor -> query conn upsertCbor (snapshotEventTableName, snapshotEventIndex, snapshotEventNumber, snapshotEventId, payload) :: IO [Only Int]
        pure $ case stored of
            [Only _] -> SnapshotStored
            [] -> NewerSnapshotRetained
            _ -> NewerSnapshotRetained

    delete :: Connection -> SnapshotKey -> StoredSnapshot -> IO ()
    delete conn SnapshotKey{snapshotEventTableName, snapshotEventIndex} StoredSnapshot{storedSnapshotCheckpoint = SnapshotCheckpoint{snapshotEventNumber, snapshotEventId}, storedSnapshotTimestamp, storedSnapshotFormat, storedSnapshotPayload} =
        void $ case storedSnapshotFormat of
            SnapshotJson -> execute conn "delete from public.\"domaindriven-snapshots\" where event_table_name = ? and event_index = ? and event_number = ? and event_id = ? and timestamp = ? and snapshot_json = ?::jsonb and snapshot_cbor is null" (snapshotEventTableName, snapshotEventIndex, snapshotEventNumber, snapshotEventId, storedSnapshotTimestamp, Text.decodeUtf8 storedSnapshotPayload)
            SnapshotCbor -> execute conn "delete from public.\"domaindriven-snapshots\" where event_table_name = ? and event_index = ? and event_number = ? and event_id = ? and timestamp = ? and snapshot_json is null and snapshot_cbor = ?" (snapshotEventTableName, snapshotEventIndex, snapshotEventNumber, snapshotEventId, storedSnapshotTimestamp, storedSnapshotPayload)

snapshotDdlLock :: Int64
snapshotDdlLock = 5764758386540796211

createSnapshotTable :: Query
createSnapshotTable = "CREATE TABLE IF NOT EXISTS public.\"domaindriven-snapshots\" (event_table_name text NOT NULL CHECK (event_table_name <> ''), event_index text NOT NULL, event_number bigint NOT NULL CHECK (event_number > 0), event_id uuid NOT NULL, timestamp timestamptz NOT NULL DEFAULT now(), snapshot_json jsonb, snapshot_cbor bytea, PRIMARY KEY (event_table_name, event_index), CHECK (num_nonnulls(snapshot_json, snapshot_cbor) = 1));"

upsertJson :: Query
upsertJson = "insert into public.\"domaindriven-snapshots\" (event_table_name, event_index, event_number, event_id, snapshot_json, snapshot_cbor) values (?, ?, ?, ?, ?::jsonb, null) on conflict (event_table_name, event_index) do update set event_number = excluded.event_number, event_id = excluded.event_id, timestamp = now(), snapshot_json = excluded.snapshot_json, snapshot_cbor = null where public.\"domaindriven-snapshots\".event_number <= excluded.event_number returning 1"

upsertCbor :: Query
upsertCbor = "insert into public.\"domaindriven-snapshots\" (event_table_name, event_index, event_number, event_id, snapshot_json, snapshot_cbor) values (?, ?, ?, ?, null, ?) on conflict (event_table_name, event_index) do update set event_number = excluded.event_number, event_id = excluded.event_id, timestamp = now(), snapshot_json = null, snapshot_cbor = excluded.snapshot_cbor where public.\"domaindriven-snapshots\".event_number <= excluded.event_number returning 1"
