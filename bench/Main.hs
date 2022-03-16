{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Action
import           Criterion
import           Criterion.Main
import qualified Database.PostgreSQL.Simple                   as PG
import           DomainDriven
import           DomainDriven.Internal.Class    ( toStored )
import           DomainDriven.Persistance.PostgresIORefState
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

setupDbCheat :: IO (PostgresEvent CounterModel CounterEvent)
setupDbCheat = do
    conn            <- getConn
    [PG.Only count] <- PG.query_ conn "select count(*) from benchmark_events_v1"
    putStrLn $ "Database contains: " <> show (count :: Int64) <> " events"
    postgresWriteModel getConn eventTable applyCounterEvent 0

setupDbFull :: Int -> IO (PostgresEvent CounterModel CounterEvent)
setupDbFull nrEvents = do
    conn   <- getConn
    _      <- PG.execute_ conn "drop table if exists benchmark_events_v1"
    _      <- postgresWriteModel getConn eventTable applyCounterEvent 0
    events <- traverse toStored
                       (take nrEvents $ cycle [CounterIncreased, CounterDecreased])
    _ <- writeEvents conn (getEventTableName eventTable) events
    setupDbCheat

$(mkServer serverConfig ''CounterCmd)

main' :: IO ()
main' = do
    pg <- setupDbCheat
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
        ["list"  ] -> mainList'
        ["stream"] -> mainStream'
        _          -> do
            putStrLn $ "Crappy argument: " <> show args
            exitFailure

mainList :: IO ()
mainList = do
    pg <- setupDbCheat
    defaultMain
        [bench "read last event using getEventList" (nfIO $ last <$> getEventList pg)]

mainList' :: IO ()
mainList' = do
    pg <- setupDbCheat
    putStrLn "read last event using getEventList"
    ev <- last <$> getEventList pg
    print ev

mainStream' :: IO ()
mainStream' = do
    pg <- setupDbCheat
    putStrLn "read last event using getEventStream"
    ev <- Stream.last $ getEventStream pg
    print ev

mainStream :: IO ()
mainStream = do
    pg <- setupDbCheat
    defaultMain
        [ bench "read last event using getEventStream"
                (nfIO $ Stream.last $ getEventStream pg)
        ]
