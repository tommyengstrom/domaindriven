module DomainDriven.Persistance.Postgres.Types where

import Control.Monad.Catch
import Data.Aeson
import Data.Hashable (Hashable)
import Data.Int
import Data.Pool.Introspection as Pool
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Typeable
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.FromField qualified as FF
import DomainDriven.Persistance.Class
import GHC.Generics (Generic)
import Prelude

data PersistanceError
    = EncodingError String
    | ValueError String
    deriving (Show, Eq, Typeable, Exception)

type EventTableBaseName = String
type EventVersion = Int
type EventTableName = String
type PreviousEventTableName = String
type ChunkSize = Int

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

type EventMigration = PreviousEventTableName -> EventTableName -> Connection -> IO ()

data EventTable
    = MigrateUsing EventMigration EventTable
    | InitialVersion EventTableBaseName

newtype EventNumber = EventNumber {unEventNumber :: Int64}
    deriving (Show, Generic)
    deriving newtype (Eq, Ord, Num)

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
    , event :: Value
    }
    deriving (Show, Eq, Generic, PG.FromRow)

fromEventRow :: (FromJSON e, MonadThrow m) => EventRowOut -> m (Stored e, EventNumber)
fromEventRow (EventRowOut evKey no ts ev) = case fromJSON ev of
    Success a -> pure (Stored a ts evKey, no)
    Error err ->
        throwM
            . EncodingError
            $ "Failed to parse event "
                <> show evKey
                <> ": "
                <> err
                <> "\nWhen trying to parse:\n"
                <> show ev
