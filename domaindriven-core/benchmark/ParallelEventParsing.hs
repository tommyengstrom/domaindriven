module Main where

import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq (NFData (..), deepseq, force)
import Control.Exception (evaluate)
import Criterion.Main
import Data.Aeson
import Data.Bits (xor, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.UUID (nil)
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres.Internal (defaultReadChunkSize, parseEventRows)
import DomainDriven.Persistance.Postgres.Types
    ( EventNumber (..)
    , EventRowOut (..)
    , ParseConcurrency
    , fromEventRowResult
    )
import GHC.Generics (Generic)
import UnliftIO (pooledMapConcurrentlyN, throwIO)
import Prelude

type BenchModel = Int

newtype BenchRows = BenchRows [EventRowOut]

instance NFData BenchRows where
    rnf (BenchRows rows) = forceEventRows rows

data BenchEvent = BenchEvent
    { delta :: !Int
    , unusedChecksum :: !Int
    }
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

instance FromJSON BenchEvent where
    parseJSON = withObject "BenchEvent" $ \o -> do
        delta <- o .: "delta"
        payload <- o .: "payload"
        pure $! BenchEvent{delta, unusedChecksum = payloadChecksum payload}

data BenchPayload = BenchPayload
    { title :: !Text
    , tags :: ![Text]
    , measurements :: ![BenchMeasurement]
    , nested :: !BenchNested
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, NFData)

data BenchMeasurement = BenchMeasurement
    { metricId :: !Int
    , metricName :: !Text
    , samples :: ![Int]
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, NFData)

data BenchNested = BenchNested
    { groupName :: !Text
    , leaves :: ![BenchLeaf]
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, NFData)

data BenchLeaf = BenchLeaf
    { leafId :: !Int
    , leafName :: !Text
    , leafEnabled :: !Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, NFData)

eventCount :: Int
eventCount = 10000

payloadWidth :: Int
payloadWidth = 48

parseWorkIterations :: Int
parseWorkIterations = 4

eventTimestamp :: UTCTime
eventTimestamp = UTCTime (fromGregorian 2026 1 1) 0

-- | Worker counts to benchmark, capped at the capabilities the RTS was given
-- (@-N@). Includes high counts (14, 16) because heterogeneous cores (P/E) can
-- scale differently than low counts.
workerCounts :: Int -> [Int]
workerCounts capabilities =
    nub [max 1 (min capabilities w) | w <- [1, 4, 14, 16]]

main :: IO ()
main = do
    capabilities <- getNumCapabilities
    let expected = eventCount
    defaultMain
        [ env setupRows $ \(BenchRows rows) ->
            bgroup
                "compute state from JSON events"
                [ bgroup
                    ("workers=" <> show w)
                    [ bench "streamly" $
                        nfIO $
                            expectState expected =<< computeStateWith streamlyStrategy w rows
                    , bench "pooled" $
                        nfIO $
                            expectState expected =<< computeStateWith parseEventRowsPooled w rows
                    ]
                | w <- workerCounts capabilities
                ]
        ]

setupRows :: IO BenchRows
setupRows =
    evaluate $
        let rows = mkRows eventCount
         in forceEventRows rows `seq` BenchRows rows

mkRows :: Int -> [EventRowOut]
mkRows count =
    [ EventRowOut
        nil
        (EventNumber $ fromIntegral i)
        eventTimestamp
        (mkEventValue i)
    | i <- [1 .. count]
    ]

mkEventValue :: Int -> ByteString
mkEventValue i =
    LBS.toStrict
        . encode
        $ object
            [ "delta" .= (1 :: Int)
            , "payload" .= mkPayload i
            ]

mkPayload :: Int -> BenchPayload
mkPayload i =
    BenchPayload
        { title = T.pack ("event-" <> show i)
        , tags = [T.pack ("tag-" <> show j) | j <- [1 .. payloadWidth]]
        , measurements =
            [ BenchMeasurement
                { metricId = i * payloadWidth + j
                , metricName = T.pack ("metric-" <> show i <> "-" <> show j)
                , samples = [i, j, i + j, i * j]
                }
            | j <- [1 .. payloadWidth]
            ]
        , nested =
            BenchNested
                { groupName = T.pack ("group-" <> show (i `mod` 17))
                , leaves =
                    [ BenchLeaf
                        { leafId = i * payloadWidth + j
                        , leafName = T.pack ("leaf-" <> show i <> "-" <> show j)
                        , leafEnabled = j `mod` 2 == 0
                        }
                    | j <- [1 .. payloadWidth]
                    ]
                }
        }

forceEventRows :: [EventRowOut] -> ()
forceEventRows = foldr forceRow ()
  where
    forceRow :: EventRowOut -> () -> ()
    forceRow (EventRowOut eventId eventNumber eventTime eventValue) rest =
        eventId `seq` eventNumber `seq` eventTime `seq` eventValue `deepseq` rest

-- | A parsing strategy: turn raw rows into parsed events using @workers@ parser
-- threads. Lets the benchmark compare the streamly implementation
-- ('parseEventRows') head-to-head with a hand-rolled 'pooledMapConcurrentlyN'.
type ParseStrategy =
    ParseConcurrency -> [EventRowOut] -> IO [(Stored BenchEvent, EventNumber)]

computeStateWith :: ParseStrategy -> ParseConcurrency -> [EventRowOut] -> IO BenchModel
computeStateWith parse workers rows = do
    parsed <- parse workers rows
    let result = foldl' applyBenchEvent 0 parsed
    result `seq` pure result

-- | The streamly parser under test, fed the production default chunk size so
-- the per-task granularity (@chunkSize \`div\` workers@) matches real reads.
streamlyStrategy :: ParseStrategy
streamlyStrategy workers = parseEventRows workers defaultReadChunkSize

-- | Baseline parser using a hand-rolled thread pool, mirroring the
-- pre-streamly implementation, for head-to-head comparison.
parseEventRowsPooled :: ParseStrategy
parseEventRowsPooled workers rows = do
    parsed <-
        if workers <= 1
            then traverse parseRow rows
            else pooledMapConcurrentlyN workers parseRow rows
    either throwIO pure (sequence parsed)
  where
    parseRow = evaluate . force . fromEventRowResult @BenchEvent

applyBenchEvent :: BenchModel -> (Stored BenchEvent, EventNumber) -> BenchModel
applyBenchEvent model (stored, _) = model + delta (storedEvent stored)

expectState :: BenchModel -> BenchModel -> IO BenchModel
expectState expected actual
    | actual == expected = pure actual
    | otherwise = fail $ "Expected state " <> show expected <> ", got " <> show actual

payloadChecksum :: BenchPayload -> Int
payloadChecksum payload =
    burnChecksum parseWorkIterations (payloadShapeChecksum payload)

payloadShapeChecksum :: BenchPayload -> Int
payloadShapeChecksum BenchPayload{title, tags, measurements, nested} =
    T.length title
        + foldl' (\total tag -> total + T.length tag) 0 tags
        + foldl' (\total measurement -> total + measurementChecksum measurement) 0 measurements
        + nestedChecksum nested

burnChecksum :: Int -> Int -> Int
burnChecksum iterations seed = go iterations seed
  where
    go remaining acc
        | remaining <= 0 = acc
        | otherwise =
            let acc' = ((acc * 1664525) `xor` (acc + 1013904223)) .&. 0x3fffffff
             in acc' `seq` go (remaining - 1) acc'

measurementChecksum :: BenchMeasurement -> Int
measurementChecksum BenchMeasurement{metricId, metricName, samples} =
    metricId
        + T.length metricName
        + foldl' (+) 0 samples

nestedChecksum :: BenchNested -> Int
nestedChecksum BenchNested{groupName, leaves} =
    T.length groupName
        + foldl' (\total leaf -> total + leafChecksum leaf) 0 leaves

leafChecksum :: BenchLeaf -> Int
leafChecksum BenchLeaf{leafId, leafName, leafEnabled} =
    leafId
        + T.length leafName
        + if leafEnabled then 1 else 0
