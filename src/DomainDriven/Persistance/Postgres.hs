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
    , seed             :: model
    }
    deriving Generic


instance (FromRow m, FromRow (Stored e), ToRow m)
        => ReadModel (PostgresStateAndEvent m e) where
    type Model (PostgresStateAndEvent m e) = m
    type Event (PostgresStateAndEvent m e) = e
    applyEvent' pg = app pg
    getModel' pg = do
        conn <- getConnection pg
        r    <- query_ conn (queryState pg)
            `catch` const @_ @ResultError (pure <$> recalculateState conn)
        case r of
            []  -> recalculateState conn
            [x] -> pure x
            xs ->
                throwM
                    $  ValueError
                    $  "Invalid state query. Found "
                    <> show (length xs)
                    <> " states"
      where
        recalculateState :: Connection -> IO m
        recalculateState conn = do
            s <- foldl' (app pg) (seed pg) <$> getEvents' pg
            _ <- query conn (writeState pg) s :: IO [m] -- FIXME: Not thread safe!
            pure s


    getEvents' pg = do
        conn <- getConnection pg
        query_ conn (queryEvents pg)

instance (FromRow m, ToRow m, FromRow (Stored e), ToRow (Stored e))
        => WriteModel (PostgresStateAndEvent m e) where
    type Error (PostgresStateAndEvent m e) = PersistanceError
    transactionalUpdate' pg evalCmd = do
        conn <- getConnection pg
        withTransaction conn $ do
            m <- getModel' pg
            case evalCmd m of
                Left  err        -> throwM err
                Right (ret, evs) -> do
                    storedEvs <- traverse toStored evs
                    let newM = foldl' (app pg) m storedEvs
                        -- FIXME: Ensure the events are applied in the correct order!
                    _ <-
                        traverse (query conn (writeEvents pg)) storedEvs :: IO
                            [[Stored e]]
                    _ <- query conn (writeState pg) newM :: IO [m]
                    pure ret
