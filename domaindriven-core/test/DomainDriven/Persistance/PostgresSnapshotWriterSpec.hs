module DomainDriven.Persistance.PostgresSnapshotWriterSpec where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Exception (bracket)
import Control.Monad (forM_, void, when)
import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Data.Pool.Introspection qualified as Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres
import DomainDriven.Persistance.PostgresSnapshotSpec
    ( SnapshotTestEvent (..)
    , SnapshotTestModel (..)
    , applySnapshotEvent
    , awaitSnapshots
    , mkConnection
    , seedThreeEvents
    , snapshotConfigFor
    , snapshotCount
    , snapshotHead
    , withCleanDatabase
    , withConfiguredBackend
    , withConnection
    , within
    )
import GHC.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import UnliftIO (cancel, wait, withAsync)
import Prelude

spec :: Spec
spec = describe "PostgreSQL background snapshot writer" $ do
    it "returns committed commands and runs their hooks while storage is blocked" $
        withCleanDatabase $ \pool -> within "command and hook responsiveness" $ do
            started <- newChan
            release <- newChan
            hook <- newChan
            let store :: SnapshotStore
                store = postgresSnapshotStore pool
                config :: SnapshotConfig SnapshotTestModel
                config =
                    (snapshotConfigFor pool)
                        { snapshotStore =
                            store
                                { storeSnapshot = \key checkpoint format payload -> do
                                    writeChan started ()
                                    readChan release
                                    storeSnapshot store key checkpoint format payload
                                }
                        , snapshotTimeout = longTimeout
                        }
            withConfiguredBackend pool config applySnapshotEvent $ \backend -> do
                let hooked :: PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent
                    hooked = backend{updateHook = \_ _ _ _ -> writeChan hook ()}
                runCmd hooked NoIndex (\_ -> pure (id, replicate 3 Increment)) `shouldReturn` SnapshotTestModel 3
                readChan started
                readChan hook
                length <$> getEventList backend NoIndex `shouldReturn` 3
                flushSnapshots shortTimeout backend `shouldReturn` SnapshotFlushTimedOut
                writeChan release ()
                awaitSnapshots backend
                snapshotHead pool `shouldReturn` [3]

    it "returns cold reconstructions while their snapshot write is blocked" $
        withCleanDatabase $ \pool -> within "cold read responsiveness" $ do
            _ <- seedThreeEvents pool
            started <- newChan
            release <- newChan
            let store :: SnapshotStore
                store = postgresSnapshotStore pool
                config :: SnapshotConfig SnapshotTestModel
                config =
                    (snapshotConfigFor pool)
                        { snapshotStore =
                            store
                                { storeSnapshot = \key checkpoint format payload -> do
                                    writeChan started ()
                                    readChan release
                                    storeSnapshot store key checkpoint format payload
                                }
                        , snapshotTimeout = longTimeout
                        }
            withConfiguredBackend pool config applySnapshotEvent $ \backend -> do
                getModel backend NoIndex `shouldReturn` SnapshotTestModel 3
                readChan started
                writeChan release ()
                awaitSnapshots backend
                snapshotHead pool `shouldReturn` [3]

    it "encodes off the request thread" $
        withCleanDatabase $ \pool -> within "encoding responsiveness" $ do
            started <- newChan
            release <- newChan
            let config :: SnapshotConfig SnapshotTestModel
                config =
                    (snapshotConfigFor pool)
                        { snapshotCodec = customSnapshotCodec
                            (fromJust $ mkSnapshotCodecId "paused-json-v1")
                            SnapshotJson
                            (pausedEncode started release)
                            eitherDecodeStrict'
                        , snapshotTimeout = longTimeout
                        }
            withConfiguredBackend pool config applySnapshotEvent $ \backend -> do
                withAsync (runCmd backend NoIndex $ \_ -> pure (id, replicate 3 Increment)) $ \command -> do
                    readChan started
                    wait command `shouldReturn` SnapshotTestModel 3
                    writeChan release ()
                    awaitSnapshots backend
                    snapshotHead pool `shouldReturn` [3]

    it "does not cancel accepted work when its triggering request is cancelled" $
        withCleanDatabase $ \pool -> within "request cancellation" $ do
            started <- newChan
            release <- newChan
            returned <- newChan
            holdRequest <- newChan
            let store :: SnapshotStore
                store = postgresSnapshotStore pool
                config :: SnapshotConfig SnapshotTestModel
                config =
                    (snapshotConfigFor pool)
                        { snapshotStore =
                            store
                                { storeSnapshot = \key checkpoint format payload -> do
                                    writeChan started ()
                                    readChan release
                                    storeSnapshot store key checkpoint format payload
                                }
                        , snapshotTimeout = longTimeout
                        }
            withConfiguredBackend pool config applySnapshotEvent $ \backend -> do
                let request :: IO ()
                    request = do
                        void $ runCmd backend NoIndex $ \_ -> pure (id, replicate 3 Increment)
                        writeChan returned ()
                        readChan holdRequest
                withAsync request $ \thread -> do
                    readChan returned
                    readChan started
                    cancel thread
                    writeChan release ()
                    awaitSnapshots backend
                    snapshotHead pool `shouldReturn` [3]
                    length <$> getEventList backend NoIndex `shouldReturn` 3

    it "coalesces full-queue updates, preserves FIFO order, and retries dropped indexes on later traffic" $
        withCleanDatabase $ \pool -> within "bounded FIFO queue" $ do
            started <- newChan
            release <- newChan
            firstWrite <- newIORef True
            writes <- newIORef ([] :: [(Text, Int64)])
            let store :: SnapshotStore
                store = postgresSnapshotStore pool
                config :: SnapshotConfig SnapshotTestModel
                config =
                    (snapshotConfigFor pool)
                        { snapshotFrequency = fromJust $ mkEveryNEvents 1
                        , snapshotQueueCapacity = fromJust $ mkSnapshotQueueCapacity 3
                        , snapshotTimeout = longTimeout
                        , snapshotStore =
                            store
                                { storeSnapshot = \key checkpoint format payload -> do
                                    atomicModifyIORef' writes $ \previous -> (previous <> [(snapshotEventIndex key, snapshotEventCount checkpoint)], ())
                                    shouldBlock <- atomicModifyIORef' firstWrite $ \first -> (False, first)
                                    when shouldBlock $ writeChan started () >> readChan release
                                    storeSnapshot store key checkpoint format payload
                                }
                        }
            withConfiguredBackend pool config applySnapshotEvent $ \backend -> do
                let increment :: Text -> IO ()
                    increment index = void $ runCmd backend (Indexed index) $ \_ -> pure (id, [Increment])
                increment "active"
                readChan started
                forM_ ["b", "c", "active", "active", "b", "b", "d"] increment
                readIORef writes `shouldReturn` [("active", 1)]
                writeChan release ()
                awaitSnapshots backend
                readIORef writes `shouldReturn` [("active", 1), ("b", 3), ("c", 1), ("active", 3)]
                snapshotCount pool `shouldReturn` 3
                getModel backend (Indexed "d") `shouldReturn` SnapshotTestModel 1
                awaitSnapshots backend
                readIORef writes `shouldReturn` [("active", 1), ("b", 3), ("c", 1), ("active", 3), ("d", 1)]

    it "closes idempotently, discards pending work, and releases an active store connection" $
        withCleanDatabase $ \pool -> within "writer shutdown" $ do
            let acquirePool :: IO (Pool.Pool Connection)
                acquirePool = Pool.newPool $ Pool.setNumStripes (Just 1) $ Pool.defaultPoolConfig mkConnection close 60 1
            bracket acquirePool Pool.destroyAllResources $ \snapshotPool -> do
                started <- newChan
                holdStore <- newChan :: IO (Chan ())
                attempts <- newIORef (0 :: Int)
                let store :: SnapshotStore
                    store = postgresSnapshotStore snapshotPool
                    config :: SnapshotConfig SnapshotTestModel
                    config =
                        (snapshotConfigFor pool)
                            { snapshotFrequency = fromJust $ mkEveryNEvents 1
                            , snapshotTimeout = longTimeout
                            , snapshotStore =
                                store
                                    { storeSnapshot = \_ _ _ _ -> withConnection snapshotPool $ \_ -> do
                                        atomicModifyIORef' attempts $ \count -> (count + 1, ())
                                        writeChan started ()
                                        readChan holdStore
                                        pure SnapshotStored
                                    }
                            }
                withConfiguredBackend pool config applySnapshotEvent $ \backend -> do
                    getModel backend (Indexed "pending") `shouldReturn` SnapshotTestModel 0
                    void $ runCmd backend (Indexed "active") $ \_ -> pure (id, [Increment])
                    readChan started
                    void $ runCmd backend (Indexed "pending") $ \_ -> pure (id, [Increment])
                    closeSnapshotWriter backend
                    closeSnapshotWriter backend
                    flushSnapshots defaultSnapshotTimeout backend `shouldReturn` SnapshotFlushClosed
                    readIORef attempts `shouldReturn` 1
                    withConnection snapshotPool (\conn -> query_ conn "select 1" :: IO [Only Int]) `shouldReturn` [Only 1]
                    runCmd backend (Indexed "active") (\_ -> pure (id, [Increment])) `shouldReturn` SnapshotTestModel 2
                    length <$> getEventList backend (Indexed "pending") `shouldReturn` 1
                    readIORef attempts `shouldReturn` 1

    it "logs failures using the triggering backend and restarts on later traffic" $
        withCleanDatabase $ \pool -> within "failure and idle restart" $ do
            attempts <- newIORef (0 :: Int)
            messages <- newIORef ([] :: [String])
            let store :: SnapshotStore
                store = postgresSnapshotStore pool
                config :: SnapshotConfig SnapshotTestModel
                config =
                    (snapshotConfigFor pool)
                        { snapshotStore =
                            store
                                { storeSnapshot = \key checkpoint format payload -> do
                                    attempt <- atomicModifyIORef' attempts $ \count -> (count + 1, count)
                                    if attempt == 0 then fail "first store failed" else storeSnapshot store key checkpoint format payload
                                }
                        }
            withConfiguredBackend pool config applySnapshotEvent $ \backend -> do
                let logged :: PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent
                    logged = backend{logger = \entry -> atomicModifyIORef' messages $ \previous -> (show entry : previous, ())}
                void $ runCmd logged NoIndex $ \_ -> pure (id, replicate 3 Increment)
                awaitSnapshots backend
                snapshotCount pool `shouldReturn` 0
                readIORef messages >>= (`shouldSatisfy` any (isInfixOf "first store failed"))
                readIORef attempts `shouldReturn` 1
                getModel backend NoIndex `shouldReturn` SnapshotTestModel 3
                awaitSnapshots backend
                snapshotHead pool `shouldReturn` [3]
                readIORef attempts `shouldReturn` 2
                forM_ [6, 9 .. 30] $ \count -> do
                    void $ runCmd backend NoIndex $ \_ -> pure (id, replicate 3 Increment)
                    awaitSnapshots backend
                    snapshotHead pool `shouldReturn` [count]

    it "keeps processing after timeouts and failing failure loggers" $
        withCleanDatabase $ \pool -> within "timeout recovery" $ do
            holdStore <- newChan
            firstWrite <- newIORef True
            let store :: SnapshotStore
                store = postgresSnapshotStore pool
                config :: SnapshotConfig SnapshotTestModel
                config =
                    (snapshotConfigFor pool)
                        { snapshotTimeout = fromJust $ mkSnapshotTimeout 0.2
                        , snapshotStore =
                            store
                                { storeSnapshot = \key checkpoint format payload -> do
                                    shouldBlock <- atomicModifyIORef' firstWrite $ \first -> (False, first)
                                    when shouldBlock $ readChan holdStore
                                    storeSnapshot store key checkpoint format payload
                                }
                        }
            withConfiguredBackend pool config applySnapshotEvent $ \backend -> do
                let logged :: PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent
                    logged = backend{logger = \entry -> when ("SnapshotOperationFailure" `isInfixOf` show entry) $ fail "logger failed"}
                void $ runCmd logged NoIndex $ \_ -> pure (id, replicate 3 Increment)
                awaitSnapshots backend
                snapshotCount pool `shouldReturn` 0
                getModel logged NoIndex `shouldReturn` SnapshotTestModel 3
                awaitSnapshots backend
                snapshotHead pool `shouldReturn` [3]

shortTimeout :: SnapshotTimeout
shortTimeout = fromJust $ mkSnapshotTimeout 0.01

longTimeout :: SnapshotTimeout
longTimeout = fromJust $ mkSnapshotTimeout 30

pausedEncode :: Chan () -> Chan () -> SnapshotTestModel -> ByteString
pausedEncode started release model = unsafePerformIO $ do
    writeChan started ()
    readChan release
    pure $ LBS.toStrict $ encode model
{-# NOINLINE pausedEncode #-}
