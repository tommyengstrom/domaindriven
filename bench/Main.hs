{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Action
import           Criterion
import           Criterion.Main
import           Criterion.Measurement
import           Criterion.Measurement.Types    ( Measured(..) )
import qualified Database.PostgreSQL.Simple                   as PG
import           DomainDriven
import           DomainDriven.Internal.Class    ( toStored )
import           DomainDriven.Persistance.PostgresIORefState
import           DomainDriven.Server
import           Prelude
import qualified Streamly.Prelude                             as Stream
import           Text.Pretty.Simple             ( pPrint )

getConn :: IO PG.Connection
getConn = PG.connect $ PG.ConnectInfo { connectHost     = "localhost"
                                      , connectPort     = 5432
                                      , connectUser     = "postgres"
                                      , connectPassword = "postgres"
                                      , connectDatabase = "domaindriven-benchmark"
                                      }

eventTable :: EventTable
eventTable = InitialVersion "benchmark_events"

nrEvents :: Int
nrEvents = 1000000

setupDb :: IO (PostgresEvent CounterModel CounterEvent)
setupDb = do
    conn   <- getConn
    _      <- PG.execute_ conn "drop table if exists benchmark_events"
    p      <- postgresWriteModel getConn eventTable applyCounterEvent 0
    events <- traverse toStored (take nrEvents $ repeat CounterIncreased)
    _      <- writeEvents conn (getEventTableName eventTable) events
    pure p


$(mkServer serverConfig ''CounterCmd)

main' :: IO ()
main' = do
    pg <- setupDb
    defaultMain
        [ bench ("getModel " <> show nrEvents <> " events")
                (nfIO $ last <$> getEventList pg)
        , bench ("getModel " <> show nrEvents <> " events")
                (nfIO $ Stream.last $ getEventStream pg)
        ]

main :: IO ()
main = do
    pg <- setupDb
    putStrLn $ "Using " <> show nrEvents <> " events"
    putStrLn "getEventList"
    (r1, _) <- measure (nfIO $ last <$> getEventList pg) 2
    pPrint r1

    putStrLn "getEventStream"
    (r2, _) <- measure (nfIO $ Stream.last $ getEventStream pg) 2
    pPrint r2

    print $ measAllocated r1 - measAllocated r2
