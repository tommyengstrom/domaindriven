module DomainDriven.Persistance.PostgresSpec where

import           RIO
import           DomainDriven
import           DomainDriven.Persistance.Postgres
import           Test.Hspec
import           Database.PostgreSQL.Simple
import qualified StoreModel as Store


spec :: Spec
spec = pure ()

mkTestConn :: IO Connection
mkTestConn = connect $ ConnectInfo { connectHost     = "localhost"
                                   , connectPort     = 5432
                                   , connectUser     = "postgres"
                                   , connectPassword = "postgres"
                                   , connectDatabase = "domaindriven"
                                   }
p :: PostgresStateAndEvent Store.StoreModel Store.StoreEvent
p = simplePostgres mkTestConn "events" "state" Store.applyStoreEvent mempty

createEventTable :: Connection -> IO Int64
createEventTable conn =
    execute_ conn "create table events \
            \( id uuid not null\
            \, timestamp timestamptz not null default now()\
            \, event jsonb not null\
            \);"

createStateTable :: Connection -> IO Int64
createStateTable conn =
    execute_ conn "create table states \
            \( model text not null\
            \, timestamp timestamptz not null default now()\
            \, state jsonb not null\
            \);"
