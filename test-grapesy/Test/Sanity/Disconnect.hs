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
module Test.Sanity.Disconnect where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Either
import Data.IORef
import Data.Word
import Foreign.C.Types (CInt(..))
import System.Posix
import Test.Tasty
import Test.Tasty.HUnit
import Text.Read

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Binary
import Network.GRPC.Common
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Binary
import Network.GRPC.Spec
import Test.Util
import Test.Util.RawTestServer

tests :: TestTree
tests = testGroup "Test.Sanity.Disconnect" [
      testCase "client" test_clientDisconnect
    , testCase "server" test_serverDisconnect
    ]

-- | Two separate clients make many concurrent calls, one of them disconnects.
test_clientDisconnect :: Assertion
test_clientDisconnect = do
    -- Create the server
    disconnectCounter1 <- newIORef 0
    disconnectCounter2 <- newIORef 0
    server <-
      Server.mkGrpcServer def [
          Server.someRpcHandler $
              Server.mkRpcHandler @Trivial  $ echoHandler (Just disconnectCounter1)
        , Server.someRpcHandler $
              Server.mkRpcHandler @Trivial' $ echoHandler (Just disconnectCounter2)
        ]

    portSignal <- newEmptyMVar
    void $ forkIO $ rawTestServer (pure Nothing) (putMVar portSignal) server

    -- Start server
    serverPort <- readMVar portSignal
    let serverAddress =
          Client.ServerInsecure Client.Address {
              addressHost      = "127.0.0.1"
            , addressPort      = serverPort
            , addressAuthority = Nothing
            }

    -- Start a client in a separate process
    void $ forkProcess $
      Client.withConnection def serverAddress $ \conn -> do
        -- Make 50 concurrent calls. 49 of them sending infinite messages. One
        -- of them kills this client process after 100 messages.
        let numCalls   = 50
            predicate  = pure . const False
            predicates =
              replicate (numCalls - 1) predicate ++
                [ \n -> do
                    when (n == 100) $ c_exit 1
                    return False
                ]
        mapConcurrently_
          (   Client.withRPC conn def (Proxy @Trivial)
            . countUntil
          )
          predicates

    -- Start two more clients that make 50 calls to each handler, all calls
    -- counting up to 1000
    let numCalls   = 50
        countTo    = 100
        predicate  = pure . (>= countTo)
        predicates = replicate numCalls predicate
    (result1, result2) <- concurrently
      ( Client.withConnection def serverAddress $ \conn -> do
          sum <$> mapConcurrently
            (   Client.withRPC conn def (Proxy @Trivial)
              . countUntil
            )
            predicates
      )
      ( Client.withConnection def serverAddress $ \conn -> do
          sum <$> mapConcurrently
            (   Client.withRPC conn def (Proxy @Trivial')
              . countUntil
            )
            predicates
      )

    -- All calls should have finished with a results of 'countTo', for both
    -- clients
    assertBool "" (result1 + result2 == 2 * sum (replicate numCalls countTo))

    -- We should also see only 50 client disconnects for the first handler and
    -- none for the second
    clientDisconnects1 <- readIORef disconnectCounter1
    clientDisconnects2 <- readIORef disconnectCounter2
    assertBool "" (clientDisconnects1 == 50 && clientDisconnects2 == 0)

-- | Client makes many concurrent calls, server disconnects
test_serverDisconnect :: Assertion
test_serverDisconnect = withTemporaryFile $ \ipcFile -> do
    -- We use a temporary file as a very rudimentary means of inter-process
    -- communication so the server (which runs in a separate process) can make
    -- the client aware of the port it is assigned by the OS. This also helps us
    -- make sure the server binds to the same port when it comes back up for
    -- reconnect purposes.
    let ipcWrite :: String -> IO ()
        ipcWrite msg = do
          writeFile ipcFile ""
          writeFile ipcFile msg

        ipcRead :: IO String
        ipcRead = readFile ipcFile

        ipcWaitRead :: IO String
        ipcWaitRead = do
          ipcRead >>= \case
            "" -> do
              threadDelay 10000 >> ipcWaitRead
            msg -> do
              return msg

    -- Create the server
    server <-
      Server.mkGrpcServer def [
          Server.someRpcHandler $
              Server.mkRpcHandler @Trivial $ echoHandler Nothing
        ]

    let -- Starts the server in a new process. Gives back an action that kills
        -- the server process.
        startServer :: IO (IO ())
        startServer = do
          serverPid <-
            forkProcess $
              rawTestServer (readMaybe <$> ipcRead) (ipcWrite . show) server
          return $ c_kill (fromIntegral serverPid) sigKILL

    -- Start server, get the port
    killServer    <- startServer
    serverPort    <- read <$> ipcWaitRead
    signalRestart <- newEmptyMVar
    let serverAddress =
          Client.ServerInsecure Client.Address {
              addressHost      = "127.0.0.1"
            , addressPort      = serverPort
            , addressAuthority = Nothing
            }

        reconnectPolicy :: Client.ReconnectPolicy
        reconnectPolicy = go 0
          where
            go :: Int -> Client.ReconnectPolicy
            go n
              | n == 5
              = Client.ReconnectAfter $ do
                  killRestarted <- startServer
                  putMVar signalRestart killRestarted
                  return $ Client.exponentialBackoff threadDelay 1 (1, 1) 100
              | otherwise
              = Client.ReconnectAfter $ do
                  threadDelay 10000
                  return $ go (n + 1)

        connParams :: Client.ConnParams
        connParams = def { Client.connReconnectPolicy = reconnectPolicy }

    Client.withConnection connParams serverAddress $ \conn -> do
      -- Make 50 concurrent calls. 49 of them sending infinite messages. One
      -- of them kills the server after 100 messages.
      let numCalls   = 50
          predicate  = pure . const False
          predicates =
            replicate (numCalls - 1) predicate ++
              [ \n -> do
                  when (n == 100) killServer
                  return False
              ]
      results <-
        mapConcurrently
          (   try @Client.ServerDisconnected
            . Client.withRPC conn def (Proxy @Trivial)
            . countUntil
          )
          predicates

      -- All calls should have failed
      assertBool "" (null (rights results) && length (lefts results) == numCalls)

      -- New calls should succeed (after reconnection)
      killRestarted <- takeMVar signalRestart
      result <- Client.withRPC conn def (Proxy @Trivial) $
        countUntil (pure . (>= 100))
      assertBool "" (result == 100)

      -- Do not leave the server process hanging around
      killRestarted

{-------------------------------------------------------------------------------
  Client and handler functions
-------------------------------------------------------------------------------}

-- | Send increasing numbers to the server until it responds with one that
-- satisfies the given predicate.
countUntil :: forall rpc.
  ( Input  rpc ~ Lazy.ByteString
  , Output rpc ~ Lazy.ByteString
  , ResponseTrailingMetadata rpc ~ NoMetadata
  ) => (Word64 -> IO Bool) -> Client.Call rpc -> IO Word64
countUntil = go 0
  where
    go :: Word64 -> (Word64 -> IO Bool) -> Client.Call rpc -> IO Word64
    go next p call = do
        sat <- p next
        if sat then do
          Binary.sendFinalInput @Word64 call next
          (final, NoMetadata) <- Binary.recvFinalOutput @Word64 call
          return final
        else do
          Binary.sendNextInput @Word64 call next
          next' <- Binary.recvNextOutput @Word64 call
          go (succ next') p call

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

foreign import ccall unsafe "kill" c_kill :: CInt -> CInt -> IO ()
foreign import ccall unsafe "exit" c_exit :: CInt -> IO ()

type Trivial  = RawRpc "trivial" "trivial"
type Trivial' = RawRpc "trivial" "trivial'"

type instance RequestMetadata          Trivial  = NoMetadata
type instance ResponseInitialMetadata  Trivial  = NoMetadata
type instance ResponseTrailingMetadata Trivial  = NoMetadata
type instance RequestMetadata          Trivial' = NoMetadata
type instance ResponseInitialMetadata  Trivial' = NoMetadata
type instance ResponseTrailingMetadata Trivial' = NoMetadata
