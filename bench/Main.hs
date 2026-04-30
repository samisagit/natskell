{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception     (evaluate)
import           Control.Monad         (when)
import           Data.Bits             (xor)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import           Data.Int              (Int64)
import           Data.List             (foldl')
import           Data.Maybe            (fromMaybe)
import           Data.Word             (Word32, Word64)
import           GHC.Clock             (getMonotonicTimeNSec)
import           GHC.Stats
import           Numeric               (showFFloat)
import           Parser.API
    ( ParseStep (DropPrefix, Emit, NeedMore, Reject)
    , ParsedMessage (ParsedErr, ParsedInfo, ParsedMsg, ParsedOk, ParsedPing, ParsedPong)
    , ParserAPI
    , parse
    )
import           Parser.Attoparsec     (parserApi)
import           System.Environment    (getArgs)
import           System.Exit           (die)
import           System.Mem            (performGC)
import           Text.Printf           (printf)
import           Types.Err             (Err (..))
import           Types.Info
    ( client_id
    , connect_urls
    , go
    , host
    , max_payload
    , nonce
    , port
    , proto
    , server_id
    , version
    )
import           Types.Msg             (Msg (..))

data Config = Config
                { scenarioName :: String
                , repetitions  :: Int
                , scale        :: Int
                }

data Scenario = Scenario
                  { name        :: String
                  , description :: String
                  , chunks      :: [BS.ByteString]
                  , frameCount  :: Int
                  , inputBytes  :: Int
                  }

data RunSummary = RunSummary
                    { parsedFrames :: !Int
                    , droppedBytes :: !Int
                    , checksum     :: !Int
                    }

data RuntimeDelta = RuntimeDelta
                      { allocatedBytes :: !Word64
                      , copiedBytes    :: !Word64
                      , peakLiveBytes  :: !Word64
                      , peakMemBytes   :: !Word64
                      , gcCount        :: !Word32
                      , mutatorCpuNs   :: !Int64
                      , gcCpuNs        :: !Int64
                      }

data Aggregate = Aggregate
                   { totalFrames       :: !Int
                   , totalDropped      :: !Int
                   , combinedChecksums :: !Int
                   }

defaultConfig :: Config
defaultConfig =
  Config
    { scenarioName = "mixed-chunked"
    , repetitions = 5
    , scale = 256
    }

main :: IO ()
main = do
  args <- getArgs
  if "--list-scenarios" `elem` args
    then listScenarios
    else do
      config <- parseArgs defaultConfig args
      scenario <- resolveScenario config
      benchmarkScenario parserApi config scenario

listScenarios :: IO ()
listScenarios =
  mapM_ printScenario availableScenarioNames
  where
    printScenario (scenarioKey, scenarioDescription) =
      putStrLn (scenarioKey ++ " - " ++ scenarioDescription)

availableScenarioNames :: [(String, String)]
availableScenarioNames =
  [ ("mixed-aligned", "Mixed INFO/PING/PONG/MSG/HMSG/ERR stream with frame-aligned input.")
  , ("mixed-chunked", "The same mixed stream split across irregular chunk boundaries.")
  , ("mixed-chunked-noisy", "Chunked mixed stream with occasional invalid prefix bytes.")
  ]

parseArgs :: Config -> [String] -> IO Config
parseArgs config args =
  case args of
    [] ->
      pure config
    "--scenario":value:rest ->
      parseArgs config { scenarioName = value } rest
    "--repetitions":value:rest ->
      parsePositiveInt "--repetitions" value >>= \parsed ->
        parseArgs config { repetitions = parsed } rest
    "--scale":value:rest ->
      parsePositiveInt "--scale" value >>= \parsed ->
        parseArgs config { scale = parsed } rest
    flag:_ | "--" `B8.isPrefixOf` B8.pack flag ->
      die ("unknown benchmark flag: " ++ flag)
    unexpected:_ ->
      die ("unexpected benchmark argument: " ++ unexpected)

parsePositiveInt :: String -> String -> IO Int
parsePositiveInt flag value =
  case reads value of
    [(parsed, "")]
      | parsed > 0 ->
          pure parsed
    _ ->
      die (flag ++ " expects a positive integer, got: " ++ value)

resolveScenario :: Config -> IO Scenario
resolveScenario config =
  case scenarioName config of
    "mixed-aligned" ->
      pure (alignedScenario (scale config))
    "mixed-chunked" ->
      pure (chunkedScenario (scale config))
    "mixed-chunked-noisy" ->
      pure (noisyScenario (scale config))
    unknown ->
      die ("unknown scenario: " ++ unknown)

benchmarkScenario :: ParserAPI ParsedMessage -> Config -> Scenario -> IO ()
benchmarkScenario selectedParser config scenario = do
  putStrLn "parser backend: attoparsec"
  putStrLn ("scenario: " ++ name scenario)
  putStrLn ("description: " ++ description scenario)
  putStrLn ("repetitions: " ++ show (repetitions config))
  putStrLn ("frames per repetition: " ++ show (frameCount scenario))
  putStrLn ("input bytes per repetition: " ++ show (inputBytes scenario))

  warmup <- runScenario selectedParser scenario
  validateScenario scenario warmup

  performGC
  beforeStats <- snapshotStats
  startNs <- getMonotonicTimeNSec
  aggregate <- runRepetitions (repetitions config) (runScenario selectedParser scenario)
  endNs <- getMonotonicTimeNSec
  performGC
  afterStats <- snapshotStats

  let elapsedNs = endNs - startNs
      elapsedSeconds = fromIntegral elapsedNs / 1e9 :: Double
      averageSeconds = elapsedSeconds / fromIntegral (repetitions config)
      totalParsedFrames = totalFrames aggregate
      totalInputBytes = repetitions config * inputBytes scenario
      totalDroppedBytes = totalDropped aggregate
      averageRate = fromIntegral totalParsedFrames / elapsedSeconds
      averageMiBPerSecond = fromIntegral totalInputBytes / (1024 * 1024) / elapsedSeconds :: Double
      finalChecksum = combinedChecksums aggregate
      delta = diffStats beforeStats afterStats
      allocatedPerFrame = fromIntegral (allocatedBytes delta) / fromIntegral totalParsedFrames :: Double
      copiedPerFrame = fromIntegral (copiedBytes delta) / fromIntegral totalParsedFrames :: Double

  printf "average delivery rate: %.2f frames/s\n" averageRate
  printf "average parser throughput: %.2f MiB/s\n" averageMiBPerSecond
  printf "average time per repetition: %.6fs\n" averageSeconds
  putStrLn ("dropped invalid prefix bytes: " ++ show totalDroppedBytes)
  putStrLn ("checksum: " ++ show finalChecksum)
  putStrLn ("allocated bytes: " ++ show (allocatedBytes delta))
  printf "allocated bytes per frame: %.2f\n" allocatedPerFrame
  putStrLn ("copied bytes: " ++ show (copiedBytes delta))
  printf "copied bytes per frame: %.2f\n" copiedPerFrame
  putStrLn ("peak live bytes: " ++ show (peakLiveBytes delta))
  putStrLn ("peak memory in use bytes: " ++ show (peakMemBytes delta))
  putStrLn ("GC count: " ++ show (gcCount delta))
  putStrLn ("mutator CPU ms: " ++ formatMillis (mutatorCpuNs delta))
  putStrLn ("GC CPU ms: " ++ formatMillis (gcCpuNs delta))
  putStrLn "For the full RTS breakdown, rerun with `+RTS -s`."

validateScenario :: Scenario -> RunSummary -> IO ()
validateScenario scenario summary =
  when
    (parsedFrames summary /= frameCount scenario)
    ( die
        ( "benchmark corpus parse mismatch: expected "
            ++ show (frameCount scenario)
            ++ " frames but parsed "
            ++ show (parsedFrames summary)
        )
    )

runScenario :: ParserAPI ParsedMessage -> Scenario -> IO RunSummary
runScenario parserForScenario scenario =
  pure (consumeChunks parserForScenario (chunks scenario))

runRepetitions :: Int -> IO RunSummary -> IO Aggregate
runRepetitions repetitionCount action =
  go 0 (Aggregate 0 0 0)
  where
    go completed aggregate
      | completed >= repetitionCount =
          pure aggregate
      | otherwise = do
          summary <- action
          let aggregate' =
                Aggregate
                  { totalFrames = totalFrames aggregate + parsedFrames summary
                  , totalDropped = totalDropped aggregate + droppedBytes summary
                  , combinedChecksums = combinedChecksums aggregate * 16777619 + checksum summary
                  }
          evaluate (combinedChecksums aggregate')
          go (completed + 1) aggregate'

consumeChunks :: ParserAPI ParsedMessage -> [BS.ByteString] -> RunSummary
consumeChunks parserForScenario =
  go BS.empty (RunSummary 0 0 0)
  where
    go buffer summary remainingChunks =
      case remainingChunks of
        [] ->
          drain buffer summary []
        nextChunk:rest ->
          drain (buffer <> nextChunk) summary rest

    drain buffer summary remainingChunks
      | BS.null buffer =
          case remainingChunks of
            [] ->
              summary
            _ ->
              go buffer summary remainingChunks
      | otherwise =
          case parse parserForScenario buffer of
            Emit message rest
              | BS.length rest == BS.length buffer ->
                  error "parser emitted without consuming input"
              | otherwise ->
                  drain
                    rest
                    summary
                      { parsedFrames = parsedFrames summary + 1
                      , checksum = checksum summary `xor` fingerprint message
                      }
                    remainingChunks
            NeedMore ->
              case remainingChunks of
                [] ->
                  error "parser requested more input after the corpus ended"
                _ ->
                  go buffer summary remainingChunks
            DropPrefix bytesToDrop _
              | bytesToDrop <= 0 ->
                  error "parser requested a non-positive prefix drop"
              | bytesToDrop > BS.length buffer ->
                  error "parser requested dropping more bytes than available"
              | otherwise ->
                  drain
                    (BS.drop bytesToDrop buffer)
                    summary { droppedBytes = droppedBytes summary + bytesToDrop }
                    remainingChunks
            Reject reason ->
              error ("parser rejected benchmark corpus: " ++ reason)

fingerprint :: ParsedMessage -> Int
fingerprint parsedMessage =
  case parsedMessage of
    ParsedPing _ ->
      1
    ParsedPong _ ->
      2
    ParsedOk _ ->
      3
    ParsedErr err ->
      11 + BS.length (errReason err)
    ParsedInfo info ->
      17
        + BS.length (server_id info)
        + BS.length (version info)
        + BS.length (go info)
        + BS.length (host info)
        + port info
        + max_payload info
        + proto info
        + fromMaybe 0 (client_id info)
        + maybe 0 BS.length (nonce info)
        + maybe 0 length (connect_urls info)
    ParsedMsg message ->
      23
        + BS.length (subject message)
        + BS.length (sid message)
        + maybe 0 BS.length (replyTo message)
        + maybe 0 BS.length (payload message)
        + headersWeight (headers message)

headersWeight :: Maybe [(BS.ByteString, BS.ByteString)] -> Int
headersWeight Nothing = 0
headersWeight (Just pairs) =
  foldl' (\total (headerKey, headerValue) -> total + BS.length headerKey + BS.length headerValue) 0 pairs

errReason :: Err -> BS.ByteString
errReason err =
  case err of
    ErrUnknownOp reason        -> reason
    ErrRoutePortConn reason    -> reason
    ErrAuthViolation reason    -> reason
    ErrAuthTimeout reason      -> reason
    ErrInvalidProtocol reason  -> reason
    ErrMaxControlLineEx reason -> reason
    ErrErr reason              -> reason
    ErrTlsRequired reason      -> reason
    ErrStaleConn reason        -> reason
    ErrMaxConnsEx reason       -> reason
    ErrSlowConsumer reason     -> reason
    ErrMaxPayload reason       -> reason
    ErrInvalidSubject reason   -> reason
    ErrPermViolation reason    -> reason

snapshotStats :: IO RTSStats
snapshotStats = do
  enabled <- getRTSStatsEnabled
  if enabled
    then getRTSStats
    else die "RTS stats are not enabled; the benchmark must be built with -T"

diffStats :: RTSStats -> RTSStats -> RuntimeDelta
diffStats before after =
  RuntimeDelta
    { allocatedBytes = allocated_bytes after - allocated_bytes before
    , copiedBytes = copied_bytes after - copied_bytes before
    , peakLiveBytes = max_live_bytes after
    , peakMemBytes = max_mem_in_use_bytes after
    , gcCount = gcs after - gcs before
    , mutatorCpuNs = mutator_cpu_ns after - mutator_cpu_ns before
    , gcCpuNs = gc_cpu_ns after - gc_cpu_ns before
    }

formatMillis :: Int64 -> String
formatMillis nanoseconds =
  showFFloat (Just 3) (fromIntegral nanoseconds / 1e6 :: Double) "ms"

alignedScenario :: Int -> Scenario
alignedScenario scenarioScale =
  Scenario
    { name = "mixed-aligned"
    , description = "Mixed control and delivery traffic with each frame presented whole."
    , chunks = frames
    , frameCount = length frames
    , inputBytes = sum (map BS.length frames)
    }
  where
    frames = buildMixedFrames scenarioScale

chunkedScenario :: Int -> Scenario
chunkedScenario scenarioScale =
  Scenario
    { name = "mixed-chunked"
    , description = "The same mixed traffic split across irregular byte chunks."
    , chunks = chunkStream chunkPlan payload
    , frameCount = length frames
    , inputBytes = BS.length payload
    }
  where
    frames = buildMixedFrames scenarioScale
    payload = BS.concat frames

noisyScenario :: Int -> Scenario
noisyScenario scenarioScale =
  Scenario
    { name = "mixed-chunked-noisy"
    , description = "Chunked mixed traffic with occasional invalid prefix bytes to exercise resynchronization."
    , chunks = chunkStream chunkPlan payload
    , frameCount = length baseFrames
    , inputBytes = BS.length payload
    }
  where
    baseFrames = buildMixedFrames scenarioScale
    frames = injectNoise baseFrames
    payload = BS.concat frames

chunkPlan :: [Int]
chunkPlan = [17, 31, 64, 127, 257, 511, 1024, 41, 89, 2048, 13]

chunkStream :: [Int] -> BS.ByteString -> [BS.ByteString]
chunkStream plan = go (cycle plan)
  where
    go _ remaining
      | BS.null remaining =
          []
    go (nextChunkSize:rest) remaining =
      let (chunkBytes, remainingBytes) = BS.splitAt nextChunkSize remaining
       in chunkBytes : go rest remainingBytes
    go [] _ =
      error "chunk plan unexpectedly ended"

injectNoise :: [BS.ByteString] -> [BS.ByteString]
injectNoise =
  snd . foldl' inject (0 :: Int, [])
  where
    inject (index, acc) frame
      | index `mod` 29 == 0 =
          (index + 1, acc ++ ["Z", frame])
      | otherwise =
          (index + 1, acc ++ [frame])

buildMixedFrames :: Int -> [BS.ByteString]
buildMixedFrames scenarioScale =
  concatMap buildCycle [0 .. scenarioScale - 1]

buildCycle :: Int -> [BS.ByteString]
buildCycle cycleIndex =
  [ buildInfoFrame cycleIndex
  , "PING\r\n"
  , buildMsgFrame cycleIndex "bench.alpha" Nothing 0 Nothing
  , buildMsgFrame cycleIndex "bench.beta" (Just "_INBOX.reply") 32 Nothing
  , buildMsgFrame cycleIndex "bench.gamma" Nothing 128 Nothing
  , buildHMsgFrame cycleIndex "bench.delta" (Just "_INBOX.headers") 256 defaultHeaders
  , "PONG\r\n"
  , buildMsgFrame cycleIndex "bench.epsilon" Nothing 512 Nothing
  , "+OK\r\n"
  , buildMsgFrame cycleIndex "bench.zeta" (Just "_INBOX.zeta") 1024 Nothing
  , buildHMsgFrame cycleIndex "bench.eta" Nothing 1536 extendedHeaders
  , buildMsgFrame cycleIndex "bench.theta" Nothing 2048 Nothing
  , "-ERR 'Permissions Violation For Publish To bench.zeta.'\r\n"
  , buildMsgFrame cycleIndex "bench.iota" Nothing 64 Nothing
  ]

defaultHeaders :: [(BS.ByteString, BS.ByteString)]
defaultHeaders =
  [ ("Nats-Msg-Id", "msg-123456")
  , ("Trace-Id", "trace-a1b2c3d4")
  ]

extendedHeaders :: [(BS.ByteString, BS.ByteString)]
extendedHeaders =
  [ ("Nats-Msg-Id", "msg-654321")
  , ("Trace-Id", "trace-z9y8x7w6")
  , ("Content-Type", "application/json")
  , ("Client", "bench-runner")
  ]

buildInfoFrame :: Int -> BS.ByteString
buildInfoFrame cycleIndex =
  BS.concat
    [ "INFO {"
    , field "server_id" (quote "bench-server")
    , ","
    , field "version" (quote "2.10.22")
    , ","
    , field "go" (quote "1.23.0")
    , ","
    , field "host" (quote "127.0.0.1")
    , ","
    , field "port" (decimalBytes 4222)
    , ","
    , field "max_payload" (decimalBytes 1048576)
    , ","
    , field "proto" (decimalBytes 1)
    , ","
    , field "client_id" (decimalBytes (1000 + cycleIndex))
    , ","
    , field "nonce" (quote ("nonce-" <> decimalBytes cycleIndex))
    , ","
    , field "headers" "true"
    , "}\r\n"
    ]

field :: BS.ByteString -> BS.ByteString -> BS.ByteString
field key value = BS.concat [quote key, ":", value]

quote :: BS.ByteString -> BS.ByteString
quote bytes = BS.concat ["\"", bytes, "\""]

buildMsgFrame :: Int -> BS.ByteString -> Maybe BS.ByteString -> Int -> Maybe [(BS.ByteString, BS.ByteString)] -> BS.ByteString
buildMsgFrame cycleIndex subjectName replySubject payloadBytes maybeHeaders =
  case maybeHeaders of
    Nothing ->
      BS.concat
        [ "MSG "
        , subjectName
        , " "
        , sidBytes cycleIndex
        , " "
        , maybeReply replySubject
        , decimalBytes payloadBytes
        , "\r\n"
        , payloadOf cycleIndex payloadBytes
        , "\r\n"
        ]
    Just headersList ->
      buildHMsgFrame cycleIndex subjectName replySubject payloadBytes headersList

buildHMsgFrame :: Int -> BS.ByteString -> Maybe BS.ByteString -> Int -> [(BS.ByteString, BS.ByteString)] -> BS.ByteString
buildHMsgFrame cycleIndex subjectName replySubject payloadBytes headersList =
  BS.concat
    [ "HMSG "
    , subjectName
    , " "
    , sidBytes cycleIndex
    , " "
    , maybeReply replySubject
    , decimalBytes headerBytes
    , " "
    , decimalBytes (headerBytes + payloadBytes)
    , "\r\n"
    , headerBlock
    , payloadOf cycleIndex payloadBytes
    , "\r\n"
    ]
  where
    headerBlock = BS.concat ["NATS/1.0\r\n", renderHeaders headersList, "\r\n"]
    headerBytes = BS.length headerBlock

renderHeaders :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
renderHeaders =
  foldMap (\(headerKey, headerValue) -> BS.concat [headerKey, ": ", headerValue, "\r\n"])

maybeReply :: Maybe BS.ByteString -> BS.ByteString
maybeReply Nothing             = ""
maybeReply (Just replySubject) = BS.concat [replySubject, " "]

sidBytes :: Int -> BS.ByteString
sidBytes cycleIndex = decimalBytes (100000 + cycleIndex)

payloadOf :: Int -> Int -> BS.ByteString
payloadOf cycleIndex payloadBytes =
  BS.take payloadBytes (BS.concat (replicate repeats unit))
  where
    unit =
      BS.concat
        [ "payload-"
        , decimalBytes cycleIndex
        , "-abcdefghijklmnopqrstuvwxyz0123456789"
        ]
    repeats =
      max 1 ((payloadBytes `div` BS.length unit) + 1)

decimalBytes :: Int -> BS.ByteString
decimalBytes = B8.pack . show
