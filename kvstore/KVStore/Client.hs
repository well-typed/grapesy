module KVStore.Client (runKeyValueClient) where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import System.Timeout
import Text.Printf

import Network.GRPC.Client
import Network.GRPC.Common

import KVStore.API
import KVStore.API.JSON qualified as JSON
import KVStore.API.Protobuf qualified as Protobuf
import KVStore.Cmdline
import KVStore.Util.Profiling
import KVStore.Util.RandomAccessSet (RandomAccessSet)
import KVStore.Util.RandomAccessSet qualified as RandomAccessSet
import KVStore.Util.RandomGen qualified as RandomGen

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Run the client for the specified time in seconds
--
-- Print the number of RPC calls.
runKeyValueClient :: Cmdline -> IO ()
runKeyValueClient cmdline@Cmdline{cmdDuration} = do
    statsVar <- newIORef zeroStats
    void $ timeout (cmdDuration * 1_000_000) $ client cmdline statsVar
    putStr . showStats cmdline =<< readIORef statsVar

{-------------------------------------------------------------------------------
  Stats
-------------------------------------------------------------------------------}

data Stats = Stats {
      statsNumCreate   :: !Int
    , statsNumUpdate   :: !Int
    , statsNumRetrieve :: !Int
    , statsNumDelete   :: !Int
    }

zeroStats :: Stats
zeroStats = Stats {
      statsNumCreate   = 0
    , statsNumUpdate   = 0
    , statsNumRetrieve = 0
    , statsNumDelete   = 0
    }

statsTotal :: Stats -> Int
statsTotal stats = sum [
      statsNumCreate   stats
    , statsNumUpdate   stats
    , statsNumRetrieve stats
    , statsNumDelete   stats
    ]

incNumCreate, incNumUpdate, incNumRetrieve, incNumDelete :: Stats -> Stats
incNumCreate   stats = stats{statsNumCreate   = statsNumCreate   stats + 1}
incNumUpdate   stats = stats{statsNumUpdate   = statsNumUpdate   stats + 1}
incNumRetrieve stats = stats{statsNumRetrieve = statsNumRetrieve stats + 1}
incNumDelete   stats = stats{statsNumDelete   = statsNumDelete   stats + 1}

showStats :: Cmdline -> Stats -> String
showStats Cmdline{cmdDuration} stats = unlines [
      printf "Did %.3f RPCs/s" countPerSec
    , "Totals:"
    , printf "  %d CREATE"   (statsNumCreate   stats)
    , printf "  %d UPDATE"   (statsNumUpdate   stats)
    , printf "  %d RETRIEVE" (statsNumRetrieve stats)
    , printf "  %d DELETE"   (statsNumDelete   stats)
    ]
  where
    countPerSec :: Double
    countPerSec = fromIntegral (statsTotal stats) / fromIntegral cmdDuration

{-------------------------------------------------------------------------------
  Main client
-------------------------------------------------------------------------------}

-- | Connect to the server and keep calling random RPCs
--
-- Caller should create an 'IORef' initialized to 0, then call 'client' in
-- separate thread, and kill the thread after some amount of time. The number
-- of RPC calls made can then be read off from the 'IORef'.
client :: Cmdline -> IORef Stats -> IO ()
client Cmdline{
           cmdJSON
         , cmdSecure
         , cmdDisableTcpNoDelay
         , cmdPingRateLimit
         } statsVar = do
    knownKeys <- RandomAccessSet.new
    random    <- RandomGen.new

    withConnection params server $ \conn -> do
      let kvstore :: KVStore
          kvstore
            | cmdJSON   = JSON.client     conn
            | otherwise = Protobuf.client conn

      forever $ do
        -- Pick a random CRUD action to take
        command <- RandomGen.nextInt random 4

        if command == 0 then do
          doCreate kvstore knownKeys
          modifyIORef' statsVar incNumCreate
        else do
          -- If we don't know about any keys, retry with a new random action
          noKnownKeys <- RandomAccessSet.isEmpty knownKeys
          unless noKnownKeys $ do
            case command of
              1 -> do doRetrieve kvstore knownKeys
                      modifyIORef' statsVar incNumRetrieve
              2 -> do doUpdate kvstore knownKeys
                      modifyIORef' statsVar incNumUpdate
              3 -> do doDelete kvstore knownKeys
                      modifyIORef' statsVar incNumDelete
              _ -> error "impossible"
  where
    params :: ConnParams
    params = def {
          connHTTP2Settings = def {
              http2TcpNoDelay            = not cmdDisableTcpNoDelay
            , http2OverridePingRateLimit = cmdPingRateLimit
            }
        }

    server :: Server
    server
      | cmdSecure
      = ServerSecure
          NoServerValidation
          SslKeyLogNone -- Let the server write the log
          Address {
              addressHost      = "127.0.0.1"
            , addressPort      = defaultSecurePort
            , addressAuthority = Nothing
            }

      | otherwise
      = ServerInsecure $ Address {
             addressHost      = "127.0.0.1"
           , addressPort      = defaultInsecurePort
           , addressAuthority = Nothing
           }

{-------------------------------------------------------------------------------
  Access the various server features
-------------------------------------------------------------------------------}

-- | Create a random key and value
doCreate :: KVStore -> RandomAccessSet Key -> IO ()
doCreate kvstore knownKeys = markRequest "CREATE" $ do
    key   <- createRandomKey knownKeys
    value <- Value <$> randomBytes meanValueSize

    let handleGrpcException :: GrpcException -> IO ()
        handleGrpcException e =
            if grpcError e == GrpcAlreadyExists
              then putStrLn "Key already existed"
              else throwIO e

    handle handleGrpcException $
      create kvstore (key, value)

-- | Retrieve the value of a random key
doRetrieve :: KVStore -> RandomAccessSet Key -> IO ()
doRetrieve kvstore knownKeys = markRequest "RETRIEVE" $ do
    key <- RandomAccessSet.getRandomKey knownKeys

    let handleGrpcException :: GrpcException -> IO ()
        handleGrpcException e =
            if grpcError e == GrpcNotFound then do
              RandomAccessSet.remove knownKeys key
              putStrLn "Key not found"
            else
              throwIO e

    handle handleGrpcException $ do
      Value res <- retrieve kvstore key
      when (BS.length res < 1) $
        error "Invalid response"

-- | Update a random key with a random value
doUpdate :: KVStore -> RandomAccessSet Key -> IO ()
doUpdate kvstore knownKeys = markRequest "UPDATE" $ do
    key   <- RandomAccessSet.getRandomKey knownKeys
    value <- Value <$> randomBytes meanValueSize

    let handleGrpcException :: GrpcException -> IO ()
        handleGrpcException e =
            if grpcError e == GrpcNotFound then do
              RandomAccessSet.remove knownKeys key
              putStrLn "Key not found"
            else
              throwIO e

    handle handleGrpcException $
      update kvstore (key, value)

-- | Delete the value of a random key
doDelete :: KVStore -> RandomAccessSet Key -> IO ()
doDelete kvstore knownKeys = markRequest "DELETE" $ do
    key <- RandomAccessSet.getRandomKey knownKeys
    delete kvstore key
    RandomAccessSet.remove knownKeys key

{-------------------------------------------------------------------------------
  Creating random keys and values
-------------------------------------------------------------------------------}

meanKeySize, meanValueSize :: Int
meanKeySize   = 64
meanValueSize = 65536

-- | Create and add a key to the set of known keys
createRandomKey :: RandomAccessSet Key -> IO Key
createRandomKey knownKeys = loop
  where
    loop :: IO Key
    loop = do
      key   <- Key <$> randomBytes meanKeySize
      added <- RandomAccessSet.add knownKeys key
      if added
        then return key
        else loop

-- | Create an exponentially sized byte string with a mean size
randomBytes :: Int -> IO ByteString
randomBytes mean = do
    random <- RandomGen.new
    d      <- RandomGen.nextDouble random
    let size :: Int
        size = round (fromIntegral mean * (-1 * log (1 - d)))
    RandomGen.getRandomBytes <$> RandomGen.nextBytes random (1 + size)

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

markRequest :: String -> IO a -> IO a
markRequest label =
    bracket_ (markEvent $ "CLIENT start " ++ label)
             (markEvent $ "CLIENT stop "  ++ label)