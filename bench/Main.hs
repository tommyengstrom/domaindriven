{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Action
import           Control.Lens                   ( (&)
                                                , (.~)
                                                )
import           Criterion
import           Criterion.Main
import           Data.Generics.Product
import qualified Database.PostgreSQL.Simple                   as PG
import           DomainDriven
import           DomainDriven.Internal.Class    ( toStored )
import           DomainDriven.Persistance.Postgres
import           DomainDriven.Persistance.Postgres.Internal
                                                ( getEventTableName
                                                , refreshModel
                                                , withIOTrans
                                                , writeEvents
                                                )
import           DomainDriven.Server
import           GHC.Int                        ( Int64 )
import           Prelude
import qualified Streamly.Prelude                             as Stream
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )
-- import           Text.Pretty.Simple             ( pPrint )

getConn :: IO PG.Connection
getConn = PG.connect $ PG.ConnectInfo { connectHost     = "localhost"
                                      , connectPort     = 5432
                                      , connectUser     = "postgres"
                                      , connectPassword = "postgres"
                                      , connectDatabase = "domaindriven-benchmark"
                                      }

eventTable :: EventTable
eventTable = InitialVersion "benchmark_events"

setupDbQuick :: Maybe Int -> IO (PostgresEvent CounterModel CounterEvent)
setupDbQuick mChunkSize = do
    conn            <- getConn
    [PG.Only count] <- PG.query_ conn "select count(*) from benchmark_events_v1"
    putStrLn $ "Database contains: " <> show (count :: Int64) <> " events"
    pg <- postgresWriteModel getConn eventTable applyCounterEvent 0
    pure $ case mChunkSize of
        Just s  -> pg & field @"chunkSize" .~ s
        Nothing -> pg

setupDbFull :: Int -> IO (PostgresEvent CounterModel CounterEvent)
setupDbFull nrEvents = do
    conn   <- getConn
    _      <- PG.execute_ conn "drop table if exists benchmark_events_v1"
    _      <- postgresWriteModel getConn eventTable applyCounterEvent 0
    events <- traverse toStored (take nrEvents $ cycle [CounterIncreased])
    _      <- writeEvents conn (getEventTableName eventTable) events
    setupDbQuick Nothing

$(mkServer serverConfig ''CounterCmd)

main' :: IO ()
main' = do
    pg <- setupDbQuick Nothing
    defaultMain
        [ bench "getEventList"   (nfIO $ last <$> getEventList pg)
        , bench "getEventStream" (nfIO $ Stream.last $ getEventStream pg)
        ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            mainList
            mainStream
        ["seed", i] -> do
            _ <- setupDbFull (read i)
            pure ()
        ["getLastEvent", "list"]      -> getLastEventListBench
        ["getLastEvent", "stream", i] -> getLastEventStreamBench (read i)
        ["getLastEvent", "stream"]    -> getLastEventStreamBench 50
        ["refreshModel", "list"  ]    -> refreshModelList
        ["refreshModel", "stream"]    -> refreshModelStream
        ["foldModel"   , "stream"]    -> foldModelStreamBench
        _                             -> do
            putStrLn $ "Crappy argument: " <> show args
            exitFailure

mainList :: IO ()
mainList = do
    pg <- setupDbQuick Nothing
    defaultMain
        [bench "read last event using getEventList" (nfIO $ last <$> getEventList pg)]

refreshModelList :: IO ()
refreshModelList = do
    pg <- setupDbQuick Nothing
    putStrLn "getModel"
    (m, _) <- withIOTrans pg $ refreshModel
    print m


refreshModelStream :: IO ()
refreshModelStream = do
    pg <- setupDbQuick Nothing
    putStrLn "getModel"
    (m, _) <- withIOTrans pg $ refreshModel
    print m

getLastEventListBench :: IO ()
getLastEventListBench = do
    pg <- setupDbQuick Nothing
    putStrLn "read last event using getEventList"
    ev <- last <$> getEventList pg
    print ev

getLastEventStreamBench :: Int -> IO ()
getLastEventStreamBench chunkSize = do
    pg <- setupDbQuick (Just chunkSize)
    putStrLn "read last event using getEventStream"
    ev <- Stream.last $ getEventStream pg
    print ev

foldModelStreamBench :: IO ()
foldModelStreamBench = do
    pg <- setupDbQuick Nothing
    putStrLn "read last event using getEventStream"
    x <- Stream.foldl' Action.applyCounterEvent 0 $ getEventStream pg
    --x <-
    --    Stream.foldl' (\b (Stored _ ts _) -> max b ts)
    --                  (UTCTime (fromGregorian 1900 1 1) 0)
    --        $ getEventStream pg
    print x


mainStream :: IO ()
mainStream = do
    pg <- setupDbQuick Nothing
    defaultMain
        [ bench "read last event using getEventStream"
                (nfIO $ Stream.last $ getEventStream pg)
        ]
