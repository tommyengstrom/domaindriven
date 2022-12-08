module DomainDriven.Persistance.Postgres.Migration where

import Data.Aeson
import Data.Int
import Data.String
import Database.PostgreSQL.Simple as PG
import DomainDriven
import DomainDriven.Persistance.Postgres.Internal
    ( createEventTable'
    , mkEventQuery
    , mkEventStream
    )
import DomainDriven.Persistance.Postgres.Types
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Prelude as S
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
migrate1to1 conn prevTName tName f = migrate1toMany conn prevTName tName (pure . f)

migrate1toMany
    :: forall a b
     . (FromJSON a, ToJSON b)
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> [Stored b])
    -> IO ()
migrate1toMany conn prevTName tName f = do
    _ <- createEventTable' conn tName
    S.mapM_ (liftIO . writeIt)
        . S.unfoldMany Unfold.fromList
        $ S.map (f . fst)
        $ mkEventStream 1 conn (mkEventQuery prevTName)
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
    _ <- createEventTable' conn tName
    S.mapM_ (liftIO . writeIt)
        . S.unfoldMany Unfold.fromList
        . S.map snd
        $ S.scanl' (\b -> f (fst b)) (initialState, [])
        $ S.map fst
        $ mkEventStream 1 conn (mkEventQuery prevTName)
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
