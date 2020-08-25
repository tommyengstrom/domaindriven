module DomainDriven.Persistance.Postgres where

import           DomainDriven.Internal.Class
import           RIO
import           RIO.Time
import           Database.PostgreSQL.Simple


data PersistanceError = EncodingError String
    deriving (Show, Eq, Typeable, Exception)



-- | Keep the events and state in postgres!
data PostgresStateAndEvent model event = PostgresStateAndEvent
    { getConnection    :: IO Connection
    , queryEvents      :: Query
    , queryEventsAfter :: UTCTime -> Query
    , stateQuery       :: String
    , stateWrite       :: String
    }
    deriving Generic


instance PersistanceHandler (PostgresStateAndEvent model event) model event where
    getModel _ = undefined
    getEvents _ = undefined
    transactionalUpdate _pg _appEvent _evalCmd = do
        undefined
