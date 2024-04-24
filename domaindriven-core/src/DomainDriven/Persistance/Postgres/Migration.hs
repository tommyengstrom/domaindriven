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

migrateValue1to1
    :: Connection -> PreviousEventTableName -> EventTableName -> (Value -> Value) -> IO ()
migrateValue1to1 conn prevTName tName f = migrate1to1 conn prevTName tName (fmap f)

migrate1to1
    :: forall a b
     . (FromJSON a, ToJSON b)
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> Stored b)
    -> IO ()
migrate1to1 conn prevTName tName f = do
    migrate1toMany conn prevTName tName (pure . f)

migrate1toMany
    :: forall a b
     . (FromJSON a, ToJSON b)
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> [Stored b])
    -> IO ()
migrate1toMany conn prevTName tName f = do
    Stream.fold Fold.drain
        . Stream.mapM (liftIO . writeIt)
        . Stream.unfoldMany Unfold.fromList
        $ fmap (f . fst)
        $ mkEventStream 50 conn (mkEventQuery prevTName)
  where
    writeIt :: Stored b -> IO Int64
    writeIt event =
        PG.executeMany
            conn
            ( "insert into \""
                <> fromString tName
                <> "\" (id, timestamp, event) \
                   \values (?, ?, ?)"
            )
            (fmap (\x -> (storedUUID x, storedTimestamp x, encode $ storedEvent x)) [event])

migrate1toManyWithState
    :: forall a b state
     . (FromJSON a, ToJSON b)
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (state -> Stored a -> (state, [Stored b]))
    -> state
    -> IO ()
migrate1toManyWithState conn prevTName tName f initialState = do
    Stream.fold
        Fold.drain
        . Stream.mapM (liftIO . writeIt)
        . Stream.unfoldMany Unfold.fromList
        . fmap snd
        $ Stream.scan (Fold.foldl' (\b -> f (fst b)) (initialState, []))
        $ fmap fst
        $ mkEventStream 50 conn (mkEventQuery prevTName)
  where
    writeIt :: Stored b -> IO Int64
    writeIt event =
        PG.executeMany
            conn
            ( "insert into \""
                <> fromString tName
                <> "\" (id, timestamp, event) \
                   \values (?, ?, ?)"
            )
            (fmap (\x -> (storedUUID x, storedTimestamp x, encode $ storedEvent x)) [event])
