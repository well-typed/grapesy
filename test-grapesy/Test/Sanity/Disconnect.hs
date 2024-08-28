{-# OPTIONS_GHC -Wno-orphans #-}

-- | Handling of client or server disconnections occurring with ongoing RPCs on
-- a shared connection.
--
-- When a server disconnects, we expect:
--
-- 1. All current calls fail with 'Client.ServerDisconnected'
-- 2. Future calls (after reconnection) succeed
--
-- When a client disconnects, we expect:
--
-- 1. The handlers dealing with that client (i.e. on that connection) should
--    fail with 'Server.ClientDisonnected'
-- 2. Future calls (after reconnection) succeed
module Test.Sanity.Disconnect (tests) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Either
import Data.IORef
import Data.Maybe
import Data.Word
import Foreign.C.Types (CInt(..))
import Network.Socket
import System.Posix
import Test.Tasty
import Test.Tasty.HUnit
import Text.Read hiding (step)

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Binary
import Network.GRPC.Common
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Binary
import Network.GRPC.Server.Run
import Network.GRPC.Spec
import Proto.API.Trivial
import Test.Util

tests :: TestTree
tests = testGroup "Test.Sanity.Disconnect" [
      testCase "client" test_clientDisconnect
    , testCase "server" test_serverDisconnect
    ]

-- | We want two distinct handlers running at the same time, so we have two
-- trivial RPCs
type RPC1 = Trivial' "rpc1"

-- | See 'RPC1'
type RPC2 = Trivial' "rpc2"

-- | Two separate clients make many concurrent calls, one of them disconnects.
test_clientDisconnect :: Assertion
test_clientDisconnect = do
    -- Create the server
    disconnectCounter1 <- newIORef 0
    disconnectCounter2 <- newIORef 0
    server <-
      Server.mkGrpcServer def [
          Server.someRpcHandler $
              Server.mkRpcHandler @RPC1 $ echoHandler (Just disconnectCounter1)
        , Server.someRpcHandler $
              Server.mkRpcHandler @RPC2 $ echoHandler (Just disconnectCounter2)
        ]

    -- Start server
    let serverConfig = ServerConfig {
            serverInsecure = Just $ InsecureConfig {
                insecureHost = Just "127.0.0.1"
              , insecurePort = 0
              }
          , serverSecure = Nothing
          }
    portSignal <- newEmptyMVar
    void $ forkIO $ forkServer def serverConfig server $ \runningServer -> do
      putMVar portSignal =<< getServerPort runningServer
      waitServer runningServer

    -- Wait for the server to signal its port
    serverPort <- readMVar portSignal
    let serverAddress =
          Client.ServerInsecure Client.Address {
              addressHost      = "127.0.0.1"
            , addressPort      = serverPort
            , addressAuthority = Nothing
            }


    -- Start a client in a separate process
    let numCalls = 50
    void $ forkProcess $
      Client.withConnection def serverAddress $ \conn -> do
        -- Make 50 concurrent calls. 49 of them sending infinite messages. One
        -- of them kills this client process after 100 messages.
        mapConcurrently_
            (   Client.withRPC conn def (Proxy @RPC1)
              . runSteps
            )
          $ replicate (numCalls - 1) stepsInfinite ++
              [ mkClientSteps Nothing [ (100, c_exit 1) ] ]

    -- Start two more clients that make 50 calls to each handler, all calls
    -- counting up to 100
    let numSteps = 100
        steps    = replicate numCalls $ stepsN numSteps
    (result1, result2) <- concurrently
      ( Client.withConnection def serverAddress $ \conn -> do
          sum <$> mapConcurrently
            (   Client.withRPC conn def (Proxy @RPC1)
              . runSteps
            )
            steps
      )
      ( Client.withConnection def serverAddress $ \conn -> do
          sum <$> mapConcurrently
            (   Client.withRPC conn def (Proxy @RPC2)
              . runSteps
            )
            steps
      )

    -- All calls by clients in /this/ process (not the ones we killed) should
    -- have finished with a result of 'countTo'
    assertEqual ""
      (2 * sum (replicate numCalls numSteps))
      (fromIntegral $ result1 + result2)

    -- We should also see only 50 client disconnects for the first handler and
    -- none for the second
    clientDisconnects1 <- readIORef disconnectCounter1
    clientDisconnects2 <- readIORef disconnectCounter2
    assertEqual "" 50 clientDisconnects1
    assertEqual "" 0  clientDisconnects2

-- | Client makes many concurrent calls, server disconnects
test_serverDisconnect :: Assertion
test_serverDisconnect = withTemporaryFile $ \ipcFile -> do
    -- We use a temporary file as a very rudimentary means of inter-process
    -- communication so the server (which runs in a separate process) can make
    -- the client aware of the port it is assigned by the OS.
    let ipcWrite :: PortNumber -> IO ()
        ipcWrite port = do
          writeFile ipcFile (show port)

        ipcRead :: IO PortNumber
        ipcRead = do
          fmap (readMaybe @PortNumber) (readFile ipcFile) >>= \case
            Nothing -> do
              ipcRead
            Just p -> do
              writeFile ipcFile ""
              return p

    -- Create the server
    server <-
      Server.mkGrpcServer def [
          Server.someRpcHandler $
              Server.mkRpcHandler @Trivial $ echoHandler Nothing
        ]

    let serverConfig = ServerConfig {
            serverInsecure = Just $ InsecureConfig {
                insecureHost = Just "127.0.0.1"
              , insecurePort = 0
              }
          , serverSecure = Nothing
          }

        -- Starts the server in a new process. Gives back an action that kills
        -- the created server process.
        startServer :: IO (IO ())
        startServer = do
          serverPid <-
            forkProcess $
              forkServer def serverConfig server $ \runningServer -> do
                ipcWrite =<< getServerPort runningServer
                waitServer runningServer
          return $ signalProcess sigKILL serverPid

    -- Start server, get the initial port
    killServer    <- startServer
    port1         <- ipcRead
    signalRestart <- newEmptyMVar
    let serverAddress port =
          Client.ServerInsecure Client.Address {
              addressHost      = "127.0.0.1"
            , addressPort      = port
            , addressAuthority = Nothing
            }

        reconnectPolicy :: Client.ReconnectPolicy
        reconnectPolicy = go 0
          where
            go :: Int -> Client.ReconnectPolicy
            go n
              | n == 5
              = Client.ReconnectAfter Nothing $ do
                  killRestarted <- startServer
                  port2 <- ipcRead
                  putMVar signalRestart killRestarted
                  return $
                    Client.ReconnectAfter
                      (Just $ serverAddress port2)
                      (pure Client.DontReconnect)
              | otherwise
              = Client.ReconnectAfter Nothing $ do
                  threadDelay 10000
                  return $ go (n + 1)

        connParams :: Client.ConnParams
        connParams = def { Client.connReconnectPolicy = reconnectPolicy }

    Client.withConnection connParams (serverAddress port1) $ \conn -> do
      -- Make 50 concurrent calls. 49 of them sending infinite messages. One
      -- of them kills the server after 100 messages.
      let numCalls = 50
      results <-
        mapConcurrently
            (   try @Client.ServerDisconnected
              . Client.withRPC conn def (Proxy @Trivial)
              . runSteps
            )
          $ replicate (numCalls - 1) stepsInfinite ++
              [ mkClientSteps Nothing [(100, killServer)] ]

      -- All calls should have failed
      assertBool "" (null (rights results))
      assertEqual "" numCalls (length (lefts results))

      -- New calls should succeed (after reconnection)
      killRestarted <- takeMVar signalRestart
      result <-
        Client.withRPC conn def (Proxy @Trivial) $
          runSteps (stepsN numCalls)
      assertEqual "" numCalls (fromIntegral result)

      -- Do not leave the server process hanging around
      killRestarted

{-------------------------------------------------------------------------------
  Client and handler functions
-------------------------------------------------------------------------------}

-- | Execute the client steps
runSteps :: forall rpc.
  ( Input  rpc ~ Lazy.ByteString
  , Output rpc ~ Lazy.ByteString
  , ResponseTrailingMetadata rpc ~ NoMetadata
  ) => ClientStep -> Client.Call rpc -> IO Word64
runSteps =
    go 0
  where
    go :: Word64 -> ClientStep -> Client.Call rpc -> IO Word64
    go n step call = do
        case step of
          KeepGoing mact next -> do
            fromMaybe (return ()) mact
            Binary.sendNextInput @Word64 call n
            _ <- Binary.recvNextOutput @Word64 call
            go (n + 1) next call
          Done -> do
            Binary.sendFinalInput @Word64 call n
            (_, NoMetadata) <- Binary.recvFinalOutput @Word64 call
            return n

-- | Echos any input
echoHandler ::
  ( Input  rpc ~ Lazy.ByteString
  , Output rpc ~ Lazy.ByteString
  , ResponseTrailingMetadata rpc ~ NoMetadata
  ) => Maybe (IORef Int) -> Server.Call rpc -> IO ()
echoHandler disconnectCounter call = trackDisconnects disconnectCounter $ do
    Binary.recvInput @Word64 call >>= \case
      StreamElem n  -> do
        Binary.sendNextOutput @Word64 call n
        echoHandler disconnectCounter call
      FinalElem n _ -> do
        Binary.sendFinalOutput @Word64 call (n, NoMetadata)
      NoMoreElems _ -> do
        Server.sendTrailers call NoMetadata
  where
    trackDisconnects Nothing =
        id
    trackDisconnects (Just counter) =
        handle (
            \(_e :: Server.ClientDisconnected) ->
              atomicModifyIORef' counter $ \n -> (n + 1, ())
          )

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

foreign import ccall unsafe "exit" c_exit :: CInt -> IO ()

data ClientStep = KeepGoing (Maybe (IO ())) ClientStep | Done

mkClientSteps :: Maybe Int -> [(Int, IO ())] -> ClientStep
mkClientSteps = go 0
  where
    go !i mn acts
        | maybe False (i >=) mn
        = Done
        | otherwise
        = KeepGoing (lookup i acts) $ go (i + 1) mn acts

stepsN :: Int -> ClientStep
stepsN n = mkClientSteps (Just n) []

{-# INLINE stepsInfinite #-}
stepsInfinite :: ClientStep
stepsInfinite = mkClientSteps Nothing []
