{-# LANGUAGE AllowAmbiguousTypes #-}
module DomainDriven.Persistance.Postgres.Migration where

import Control.Monad
import Data.Aeson
import Data.Foldable
import Data.Int
import Data.String
import Database.PostgreSQL.Simple as PG
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres.Internal (mkEventQuery, mkEventStream)
import DomainDriven.Persistance.Postgres.Types
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.Data.Unfold qualified as Unfold
import UnliftIO (liftIO)
import Prelude

defaultChunkSize :: ChunkSize
defaultChunkSize = 100

migrateValue1to1
    :: forall index
   . IsPgIndex index
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Value -> Value)
    -> IO ()
migrateValue1to1 = migrateValue1to1' @index defaultChunkSize

migrateValue1to1'
    :: forall index
    . IsPgIndex index
    => ChunkSize
    -> Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Value -> Value)
    -> IO ()
migrateValue1to1' chunkSize conn prevTName tName f =
    migrate1to1' @index chunkSize conn prevTName tName (fmap f)

migrate1to1
    :: forall index a b
     . (FromJSON a, ToJSON b, IsPgIndex index)
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> Stored b)
    -> IO ()
migrate1to1 = migrate1to1' @index defaultChunkSize

migrate1to1'
    :: forall index a b
     . (FromJSON a, ToJSON b, IsPgIndex index)
    => ChunkSize
    -> Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> Stored b)
    -> IO ()
migrate1to1' chunkSize conn prevTName tName f = do
    migrate1toMany' @index chunkSize conn prevTName tName (pure . f)

migrate1toMany
    :: forall index a b
     . (FromJSON a, ToJSON b, IsPgIndex index)
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> [Stored b])
    -> IO ()
migrate1toMany = migrate1toMany' @index defaultChunkSize

migrate1toMany'
    :: forall index a b
     . (FromJSON a, ToJSON b, IsPgIndex index)
    => ChunkSize
    -> Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> [Stored b])
    -> IO ()
migrate1toMany' chunkSize conn prevTName tName f = do
    migrate1toManyWithState' @index chunkSize conn prevTName tName (\_ a -> ((), f a)) ()

migrate1toManyWithState
    :: forall index a b state
     . (FromJSON a, ToJSON b, IsPgIndex index)
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (state -> Stored a -> (state, [Stored b]))
    -> state
    -> IO ()
migrate1toManyWithState = migrate1toManyWithState' @index defaultChunkSize

migrate1toManyWithState'
    :: forall index a b state
     . ( FromJSON a
       , ToJSON b
       , IsPgIndex index
       )
    => ChunkSize
    -> Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (state -> Stored a -> (state, [Stored b]))
    -> state
    -> IO ()
migrate1toManyWithState' chunkSize conn prevTName tName f initialState = do
    indices <- fetchAllIndices conn prevTName :: IO [index]
    for_ indices $ \i ->
        Stream.fold (Fold.groupsOf chunkSize Fold.toList (Fold.drainMapM (liftIO . writeIt i)))
            . Stream.unfoldMany Unfold.fromList
            . fmap snd
            $ Stream.scan (Fold.foldl' (f . fst) (initialState, []))
            $ fst <$> mkEventStream chunkSize conn (mkEventQuery prevTName i)
  where
    writeIt :: index -> [Stored b] -> IO Int64
    writeIt index events =
        PG.executeMany
            conn
            ( "insert into \""
                <> fromString tName
                <> "\" (id, index, timestamp, event) \
                   \values (?, ?, ?, ?)"
            )
            ( fmap
                (\x -> (storedUUID x, toPgIndex index, storedTimestamp x, encode $ storedEvent x))
                events
            )



fetchAllIndices
    :: forall index
     . IsPgIndex index
    => Connection
    -> EventTableName
    -> IO [index]
fetchAllIndices conn etName = fmap (fromPgIndex . fromOnly) <$> PG.query_ conn q
  where
    q :: PG.Query
    q = "select distinct index from \"" <> fromString etName <> "\" order by index;"
