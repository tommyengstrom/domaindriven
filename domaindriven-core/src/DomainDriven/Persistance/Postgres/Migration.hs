{-# LANGUAGE AllowAmbiguousTypes #-}

module DomainDriven.Persistance.Postgres.Migration where

import Control.Monad
import Data.Aeson
import Data.Foldable
import Data.Int
import Data.String
import Database.PostgreSQL.Simple as PG
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres.Internal
    ( IsPgIndex (..)
    , mkEventQuery
    , mkEventStream
    )
import DomainDriven.Persistance.Postgres.Types
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.Data.Unfold qualified as Unfold
import UnliftIO (liftIO)
import Prelude

migrateValue1to1
    :: forall index
     . IsPgIndex index
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Value -> Value)
    -> IO ()
migrateValue1to1 conn prevTName tName f =
    migrate1to1 @_ @_ @index conn prevTName tName (fmap f)

migrate1to1
    :: forall a b index
     . ( FromJSON a
       , ToJSON b
       , IsPgIndex index
       )
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> Stored b)
    -> IO ()
migrate1to1 conn prevTName tName f = do
    migrate1toMany @_ @_ @index conn prevTName tName (pure . f)

migrate1toMany
    :: forall a b index
     . ( FromJSON a
       , ToJSON b
       , IsPgIndex index
       )
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> [Stored b])
    -> IO ()
migrate1toMany conn prevTName tName f = do
    indices <- fetchThemIndices conn prevTName :: IO [index]
    for_ indices $ \i ->
        Stream.fold Fold.drain
            . Stream.mapM (liftIO . writeIt i)
            . Stream.unfoldMany Unfold.fromList
            $ fmap (f . fst)
            $ mkEventStream 1 conn (mkEventQuery prevTName i)
  where
    writeIt :: index -> Stored b -> IO Int64
    writeIt index event =
        PG.executeMany
            conn
            ( "insert into \""
                <> fromString tName
                <> "\" (id, index, timestamp, event) \
                   \values (?, ?, ?, ?)"
            )
            ( fmap
                (\x -> (storedUUID x, toPgIndex index, storedTimestamp x, encode $ storedEvent x))
                [event]
            )

migrate1toManyWithState
    :: forall a b state index
     . ( FromJSON a
       , ToJSON b
       , IsPgIndex index
       )
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (state -> Stored a -> (state, [Stored b]))
    -> state
    -> IO ()
migrate1toManyWithState conn prevTName tName f initialState = do
    indices <- fetchThemIndices conn prevTName :: IO [index]
    for_ indices $ \i ->
        Stream.fold
            Fold.drain
            . Stream.mapM (liftIO . writeIt i)
            . Stream.unfoldMany Unfold.fromList
            . fmap snd
            $ Stream.scan (Fold.foldl' (\b -> f (fst b)) (initialState, []))
            $ fmap fst
            $ mkEventStream 1 conn (mkEventQuery prevTName i)
  where
    writeIt :: index -> Stored b -> IO Int64
    writeIt index event =
        PG.executeMany
            conn
            ( "insert into \""
                <> fromString tName
                <> "\" (id, index, timestamp, event) \
                   \values (?, ?, ?, ?)"
            )
            ( fmap
                (\x -> (storedUUID x, toPgIndex index, storedTimestamp x, encode $ storedEvent x))
                [event]
            )

fetchThemIndices
    :: forall index
     . IsPgIndex index
    => Connection
    -> EventTableName
    -> IO [index]
fetchThemIndices conn etName = fmap (fromPgIndex . fromOnly) <$> PG.query_ conn q
  where
    q :: PG.Query
    q = "select distinct index from \"" <> fromString etName <> "\" order by index;"
