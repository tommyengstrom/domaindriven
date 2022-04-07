module DomainDriven.Persistance.Postgres.Types where

import           Control.Monad.Catch
import           Data.Aeson
import           Data.Int
import           Data.Time
import           Data.Typeable
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple     ( Connection )
import qualified Database.PostgreSQL.Simple                   as PG
import qualified Database.PostgreSQL.Simple.FromField         as FF
import           DomainDriven.Internal.Class
import           GHC.Generics                   ( Generic )
import           Prelude

data PersistanceError
    = EncodingError String
    | ValueError String
    deriving (Show, Eq, Typeable, Exception)

type EventTableBaseName = String
type EventVersion = Int
type EventTableName = String
type PreviousEventTableName = String
type ChunkSize = Int

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
    { model       :: !m
    , eventNumber :: !EventNumber
    }
    deriving (Show, Generic)

data NumberedEvent e = NumberedEvent
    { event       :: !(Stored e)
    , eventNumber :: !EventNumber
    }
    deriving (Show, Generic)

data OngoingTransaction = OngoingTransaction
    { fromTrans :: Connection
    }


data EventRowOut = EventRowOut
    { key          :: UUID
    , commitNumber :: EventNumber
    , timestamp    :: UTCTime
    , event        :: Value
    }
    deriving (Show, Eq, Generic, PG.FromRow)

fromEventRow :: (FromJSON e, MonadThrow m) => EventRowOut -> m (Stored e, EventNumber)
fromEventRow (EventRowOut evKey no ts ev) = case fromJSON ev of
    Success a -> pure (Stored a ts evKey, no)
    Error err ->
        throwM
            .  EncodingError
            $  "Failed to parse event "
            <> show evKey
            <> ": "
            <> err
            <> "\nWhen trying to parse:\n"
            <> show ev
