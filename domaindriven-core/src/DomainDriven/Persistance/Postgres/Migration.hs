module DomainDriven.Persistance.Postgres.Migration where

import Control.Monad
import Data.Aeson
import Data.Int
import Data.String
import Database.PostgreSQL.Simple as PG
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres.Internal
    ( mkEventQuery
    , mkEventStream
    )
import DomainDriven.Persistance.Postgres.Types
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.Data.Unfold qualified as Unfold
import UnliftIO (liftIO)
import Prelude

defaultChunkSize :: ChunkSize
defaultChunkSize = 100

migrateValue1to1
    :: Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Value -> Value)
    -> IO ()
migrateValue1to1 = migrateValue1to1' defaultChunkSize

migrateValue1to1'
    :: ChunkSize
    -> Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Value -> Value)
    -> IO ()
migrateValue1to1' chunkSize conn prevTName tName f =
    migrate1to1' chunkSize conn prevTName tName (fmap f)

migrate1to1
    :: forall a b
     . (FromJSON a, ToJSON b)
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> Stored b)
    -> IO ()
migrate1to1 = migrate1to1' defaultChunkSize

migrate1to1'
    :: forall a b
     . (FromJSON a, ToJSON b)
    => ChunkSize
    -> Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> Stored b)
    -> IO ()
migrate1to1' chunkSize conn prevTName tName f = do
    migrate1toMany' chunkSize conn prevTName tName (pure . f)

migrate1toMany
    :: forall a b
     . (FromJSON a, ToJSON b)
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> [Stored b])
    -> IO ()
migrate1toMany = migrate1toMany' defaultChunkSize

migrate1toMany'
    :: forall a b
     . (FromJSON a, ToJSON b)
    => ChunkSize
    -> Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> [Stored b])
    -> IO ()
migrate1toMany' chunkSize conn prevTName tName f = do
    migrate1toManyWithState' chunkSize conn prevTName tName (\_ a -> ((), f a)) ()

migrate1toManyWithState
    :: forall a b state
     . (FromJSON a, ToJSON b)
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (state -> Stored a -> (state, [Stored b]))
    -> state
    -> IO ()
migrate1toManyWithState = migrate1toManyWithState' defaultChunkSize

migrate1toManyWithState'
    :: forall a b state
     . (FromJSON a, ToJSON b)
    => ChunkSize
    -> Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (state -> Stored a -> (state, [Stored b]))
    -> state
    -> IO ()
migrate1toManyWithState' chunkSize conn prevTName tName f initialState =
    Stream.fold (Fold.groupsOf chunkSize Fold.toList (Fold.drainMapM (liftIO . writeIt)))
        . Stream.unfoldMany Unfold.fromList
        . fmap snd
        $ Stream.scan (Fold.foldl' (\b -> f (fst b)) (initialState, []))
        $ fmap fst
        $ mkEventStream chunkSize conn (mkEventQuery prevTName)
  where
    writeIt :: [Stored b] -> IO Int64
    writeIt events =
        PG.executeMany
            conn
            ( "insert into \""
                <> fromString tName
                <> "\" (id, timestamp, event) \
                   \values (?, ?, ?)"
            )
            ( fmap
                ( \x ->
                    ( storedUUID x
                    , storedTimestamp x
                    , encode $ storedEvent x
                    )
                )
                events
            )
