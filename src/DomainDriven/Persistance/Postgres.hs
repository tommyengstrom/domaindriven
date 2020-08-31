module DomainDriven.Persistance.Postgres where

import           DomainDriven.Internal.Class
import           RIO
import           RIO.Time
import           Database.PostgreSQL.Simple


data PersistanceError
    = EncodingError String
    | ValueError String
    deriving (Show, Eq, Typeable, Exception)



-- | Keep the events and state in postgres!
data PostgresStateAndEvent model event = PostgresStateAndEvent
    { getConnection    :: IO Connection
    , queryEvents      :: Query
    , queryEventsAfter :: UTCTime -> Query
    , queryState       :: Query
    , writeState       :: Query -- ^ Insert to write the state
    , writeEvents      :: Query -- ^ Insert to write an event
    , app              :: model -> Stored event -> model
    }
    deriving Generic


--instance (FromRow model, FromRow event, ToRow model, ToRow (Stored event))
--        => PersistanceHandler (PostgresStateAndEvent model event) model event where
--    -- | getModel will always return the latest model
--    getModel pg = do
--        conn <- getConnection pg
--        r    <- query_ conn (queryState pg)
--        case r of
--            x : _ -> pure x
--            _     -> throwM $ ValueError "Found no state"
--            -- ^ FIXME: I should recalculate the state, right?
--            -- I need the inital event and the
--
--    getEvents pg = do
--        conn <- getConnection pg
--        query_ conn (queryEvents pg)
--
--    transactionalUpdate pg appEvent evalCmd = do
--        conn <- getConnection pg
--        withTransaction conn $ do
--            m <- getModel pg
--            case evalCmd m of
--                Left  err        -> throwM err
--                Right (ret, evs) -> do
--                    storedEvs <- traverse toStored evs
--                    let newM = foldl' appEvent m storedEvs
--                        -- FIXME: Ensure the events are applied in the correct order!
--                    _ <- traverse (query conn (writeEvents pg)) storedEvs :: IO [[event]]
--                    _ <- query conn (writeState pg) newM :: IO [model]
--                    pure ret



instance (FromRow m, FromRow e) => ReadModel (PostgresStateAndEvent m e) where
    type Model (PostgresStateAndEvent m e) = m
    type Event (PostgresStateAndEvent m e) = e
    applyEvent' pg = app pg
    getModel' pg = do
        conn <- getConnection pg
        r    <- query_ conn (queryState pg)
        case r of
            x : _ -> pure x
            _     -> throwM $ ValueError "Found no state" -- FIXME: calculate the state

    getEvents' pg = do
        conn <- getConnection pg
        query_ conn (queryEvents pg)
