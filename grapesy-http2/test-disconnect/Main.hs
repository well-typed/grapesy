module Main (main) where

import Control.Concurrent
import Control.Exception (fromException)
import Control.Exception qualified as Exception
import Control.Monad
import Data.Proxy
import Network.Socket
import System.IO
import Test.HUnit

import Network.GRPC.Common
import Network.GRPC.Client qualified as Client
import Network.GRPC.Server qualified as Server

import Network.GRPC.Server.Run qualified as Grapesy

import Test.Disconnect.Echo.Client
import Test.Disconnect.Echo.RPC
import Test.Disconnect.Echo.Server
import Test.Disconnect.Util.Client
import Test.Disconnect.Util.Process
import Test.Disconnect.Util.Server

{-------------------------------------------------------------------------------
  Test disconnects

  The goal of this test suite is to test what happens when a client or a server
  abruptly disconnects: does their peer notice that they disappeared? To model
  network failure, we run the node which will disconnect in a separate process,
  and then unceremoniously kill that process (without even giving the Haskell
  RTS a chance to shutdown cleanly).

  We use 'forkProcess' to start auxiliary processes, which does not seem to play
  nice with the @tasty@ framework; I am not sure why. I suspect something to do
  with threads, but I don't understand the exact reason. Tests would sometimes
  mysteriously hang depending on whether or not a timeout was interested just
  before the test would /begin/. We therefore do not use @tasty@ here.

  When a server disconnects, we expect:

  1. All current calls fail with 'Client.ServerDisconnected'
  2. Future calls (after reconnection) succeed

  When a client disconnects, we expect:

  1. The handlers dealing with that client (i.e. on that connection) should fail
     with 'Server.ClientDisconnected'
  2. Future calls (after reconnection) succeed
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    runTest "clientDisconnect" test_clientDisconnect
    runTest "serverDisconnect" test_serverDisconnect
  where
    runTest :: String -> Assertion -> IO ()
    runTest label testCase = do
        putStr $ label ++ ".. "
        testCase
        putStrLn "OK"


{-------------------------------------------------------------------------------
  Test /client/ disconnect
-------------------------------------------------------------------------------}

-- | Two separate clients make many concurrent calls, one of them disconnects.
test_clientDisconnect :: Assertion
test_clientDisconnect = do
    --
    -- Create the server
    --

    (handler1, handlerResults1) <- monitoredHandler $ handleEcho (Proxy @Echo1)
    (handler2, handlerResults2) <- monitoredHandler $ handleEcho (Proxy @Echo2)
    server <- Server.mkGrpcServer def [handler1, handler2]
    Grapesy.forkServer def serverConfig server $ \runningServer -> do
      serverAddr <- mkServerAddress <$> Grapesy.getServerPort runningServer

      --
      -- Create client
      --

      -- Start a client in a separate process
      --
      -- We will kill this process one each call has connected to the server
      --
      -- NOTES:
      --
      -- * 'forkProcess' will not copy any threads into the child process,
      --   the server runs only here.
      -- * The child process will kill itself, so we don't have to keep track
      --   of it here.
      let numCalls1 = 10
      clientChildProcess <- forkChildProcess $ do
        Client.withConnection def serverAddr $ \conn -> do
          clientThreads <- replicateM numCalls1 $
            fork_echoOnceThenWait conn (Proxy @Echo1)
          -- Wait until we are sure that all clients have started their RPC,
          -- then kill the process. This avoids race conditions and guarantees
          -- that the server will see @numCalls@ clients disconnecting.
          mapM_ waitClientConnected clientThreads
          killThisProcess

      -- Start second client (which will not be killed)
      let numCalls2 = 20
          numSteps  = 5
      childResults <- Client.withConnection def serverAddr $ \conn -> do
        rpc1 <- replicateM numCalls2 $ fork_countFrom conn (Proxy @Echo1) numSteps
        rpc2 <- replicateM numCalls2 $ fork_countFrom conn (Proxy @Echo2) numSteps
        mapM waitClientResult (rpc1 ++ rpc2)

      --
      -- Check results
      --

      void $ waitForChildProcess clientChildProcess

      (normalTerminations1, clientDisconnected1, unexpectedExceptions1) <-
        getHandlerResults maxWait handlerResults1 (numCalls1 + numCalls2)
      (normalTerminations2, clientDisconnected2, unexpectedExceptions2) <-
        getHandlerResults maxWait handlerResults2 numCalls2

      -- All calls by clients in /this/ process (not the ones we killed) should
      -- have finished normally
      assertBool ("Unexpected childResults: " ++ show childResults) $
        flip all childResults $ \case
          Right 15   -> True
          _otherwise -> False

      -- The handler for Echo1 should see @numCalls1@ client disconnects and
      -- @numCalls@ regular terminations; the handler for Echo2 should only see
      -- regular terminations.
      assertEqual "clientDisconnected1" numCalls1 $ clientDisconnected1
      assertEqual "clientDisconnected2" 0         $ clientDisconnected2
      assertEqual "normalTerminations1" numCalls2 $ normalTerminations1
      assertEqual "normalTerminations2" numCalls2 $ normalTerminations2

      -- No handler should have thrown any unexpected exceptions
      assertBool ("unexpectedExceptions1: " ++ show unexpectedExceptions1) $
        null unexpectedExceptions1
      assertBool ("unexpectedExceptions2: " ++ show unexpectedExceptions2) $
        null unexpectedExceptions2
  where
    serverConfig :: Grapesy.ServerConfig
    serverConfig = Grapesy.ServerConfig {
          serverInsecure = Just $ Grapesy.InsecureConfig {
              insecureHost = Just "127.0.0.1"
            , insecurePort = 0
            }
        , serverSecure = Nothing
        }

    mkServerAddress :: PortNumber -> Client.Server
    mkServerAddress port = Client.ServerInsecure Client.Address {
          addressHost      = "127.0.0.1"
        , addressPort      = port
        , addressAuthority = Nothing
        }

{-------------------------------------------------------------------------------
  Test /server/ disconnect
-------------------------------------------------------------------------------}

test_serverDisconnect :: Assertion
test_serverDisconnect = withIPC (Proxy @PortNumber) $ \ipc-> do
    -- Create the server in a separate process
    -- (We start and kill the server multiple times)
    let serverMain :: IO ()
        serverMain = do
            (handler, _results) <- monitoredHandler $ handleEcho (Proxy @Echo1)
            server <- Server.mkGrpcServer def [handler]
            Grapesy.forkServer def serverConfig server $ \runningServer -> do
              serverPort <- Grapesy.getServerPort runningServer
              ipcWrite ipc serverPort
              Grapesy.waitServer runningServer

    serverProcessVar <- newEmptyMVar
    let startServer :: IO Client.Server
        startServer = do
            serverProcess <- forkChildProcess $ serverMain
            serverPort    <- ipcRead ipc
            putMVar serverProcessVar serverProcess
            return $ mkServerAddress serverPort

        killServer :: IO ()
        killServer = do
            serverProcess <- takeMVar serverProcessVar
            killChildProcess serverProcess

    flip Exception.finally killServer $ do
      initServerAddr <- startServer

      -- When we kill the server, the client will notice the disconnect.
      -- We construct a reconnect policy which reboots the server.
      let reconnectPolicy :: Client.ReconnectPolicy
          reconnectPolicy = Client.ReconnectPolicy $ do
              serverAddr <- startServer
              return $ Client.DoReconnect Client.Reconnect {
                    Client.reconnectTo = Client.ReconnectToNew serverAddr
                  , Client.nextPolicy  = reconnectPolicy
                  , Client.onReconnect = def
                }

          connParams :: Client.ConnParams
          connParams = def { Client.connReconnectPolicy = reconnectPolicy }

      Client.withConnection connParams initServerAddr $ \conn -> do
        -- Make some calls, then kill the server
        do let numCalls = 10
           clientThreads <- replicateM numCalls $
             fork_echoOnceThenWait conn (Proxy @Echo1)

           -- All clients should be able to connect just fine
           connected <- mapM waitClientConnected clientThreads
           assertBool ("unexpected " ++ show connected) $
             flip all connected $ \case
               Right ()   -> True
               _otherwise -> False

           -- Once all clients have started their RPC, kill the server
           killServer

           -- All calls should now have failed
           results <- mapM waitClientResult clientThreads
           assertBool ("unexpected results: " ++ show results) $
             flip all results $ \case
               -- We cannot always distinguish clearly between getting
               -- disconnected and the server closing the connection without
               -- sending the trailers.
               Left e
                 | Just Client.ServerDisconnected{} <- fromException e
                 -> True

                 | Just e'@GrpcException{} <- fromException e
                 , grpcError e' == GrpcUnknown
                 -> True

               _otherwise
                 -> False

        -- New calls should succeed (after reconnection)
        -- (No need to open a new connection, this happens transparently)
        do let numCalls = 10
           let numSteps = 5
           clientThreads <- replicateM numCalls $
             fork_countFrom conn (Proxy @Echo1) numSteps

           results <- mapM waitClientResult clientThreads
           assertBool ("unexpected results: " ++ show results) $
             flip all results $ \case
               Right 15   -> True
               _otherwise -> False
  where
   serverConfig :: Grapesy.ServerConfig
   serverConfig = Grapesy.ServerConfig {
         serverInsecure = Just $ Grapesy.InsecureConfig {
             insecureHost = Just "127.0.0.1"
           , insecurePort = 0
           }
       , serverSecure = Nothing
       }

   mkServerAddress :: PortNumber -> Client.Server
   mkServerAddress port = Client.ServerInsecure Client.Address {
         addressHost      = "127.0.0.1"
       , addressPort      = port
       , addressAuthority = Nothing
       }

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

maxWait :: Int
maxWait = 2_000_000
