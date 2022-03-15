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
import           Prelude
import qualified Streamly.Prelude                             as Stream

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
nrEvents = 200000

setupDb :: IO (PostgresEvent CounterModel CounterEvent)
setupDb = do
    conn   <- getConn
    _      <- PG.execute_ conn "drop table if exists benchmark_events"
    p      <- postgresWriteModel getConn eventTable applyCounterEvent 0
    events <- traverse toStored (take nrEvents $ repeat CounterIncreased)
    _      <- writeEvents conn (getEventTableName eventTable) events
    pure p


$(mkServer serverConfig ''CounterCmd)

main :: IO ()
main = do
    pg <- setupDb
    defaultMain
        [ bench ("getModel " <> show nrEvents <> " events")
                (nfIO $ last <$> getEventList pg)
        , bench ("getModel " <> show nrEvents <> " events")
                (nfIO $ Stream.last $ getEventStream pg)
        ]
