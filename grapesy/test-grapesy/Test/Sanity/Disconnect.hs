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
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.IORef
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

import Proto.API.Trivial

import Test.Util

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.Disconnect" [
      testCase "client" test_clientDisconnect
    , testCase "server" test_serverDisconnect
    ]

{-------------------------------------------------------------------------------
  Disconnecting clients
-------------------------------------------------------------------------------}

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
    let numCalls = 10
    dyingChild <- forkProcess $
      Client.withConnection def serverAddress $ \conn -> do
        inLockstep conn (Proxy @RPC1) numCalls NeverTerminate $ \results _getFinal -> do
          -- Wait until we are sure that all clients have started their RPC,
          -- then kill the process. This avoids race conditions and guarantees
          -- that the server will see @numCalls@ clients disconnecting.
          _ <- waitForHistoryOfMinLen results 1
          c_exit 1

    -- Start two more clients; these will not disconnect
    let numSteps = 5
    (result1, result2) <- concurrently
      ( Client.withConnection def serverAddress $ \conn -> do
          inLockstep conn (Proxy @RPC1) numCalls (TerminateAfter numSteps) $ \_results getFinal ->
            getFinal
      )
      ( Client.withConnection def serverAddress $ \conn -> do
          inLockstep conn (Proxy @RPC2) numCalls (TerminateAfter numSteps) $ \_results getFinal ->
            getFinal
      )

    -- Wait for the forked process to terminate
    _status <- getProcessStatus True False dyingChild

    -- All calls by clients in /this/ process (not the ones we killed) should
    -- have finished normally
    let expectedResult = [
            replicate numCalls (StepOk i)
          | i <- reverse [1 .. numSteps]
          ]
    assertEqual "" expectedResult result1
    assertEqual "" expectedResult result2

    -- We should also see only @numCalls@ client disconnects for the first
    -- handler and none for the second
    clientDisconnects1 <- readIORef disconnectCounter1
    clientDisconnects2 <- readIORef disconnectCounter2
    assertEqual "" numCalls clientDisconnects1
    assertEqual "" 0        clientDisconnects2

-- We need to use this to properly simulate the execution environment crashing
-- in an unrecoverable way. In particular, we don't want to give the program a
-- chance to do any of its normal exception handling/cleanup behavior.
foreign import ccall unsafe "exit" c_exit :: CInt -> IO ()

{-------------------------------------------------------------------------------
  Disconnecting servers
-------------------------------------------------------------------------------}

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
              Server.mkRpcHandler @RPC1 $ echoHandler Nothing
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
              = Client.ReconnectAfter def $ do
                  killRestarted <- startServer
                  port2 <- ipcRead
                  putMVar signalRestart killRestarted
                  return $
                    Client.ReconnectAfter
                      (Client.ReconnectToNew $ serverAddress port2)
                      (pure Client.DontReconnect)
              | otherwise
              = Client.ReconnectAfter def $ do
                  threadDelay 10000
                  return $ go (n + 1)

        connParams :: Client.ConnParams
        connParams = def { Client.connReconnectPolicy = reconnectPolicy }

    Client.withConnection connParams (serverAddress port1) $ \conn -> do
      let numCalls = 10
      results <-
        inLockstep conn (Proxy @RPC1) numCalls NeverTerminate $ \results getFinal -> do
          -- Once all clients have started their RPC, kill the server
          _ <- waitForHistoryOfMinLen results 1
          killServer
          getFinal

      -- All calls should have failed (but we don't know in which step)
      assertEqual "" numCalls $ length $ filter stepFailed (concat results)

      -- New calls should succeed (after reconnection)
      killRestarted <- takeMVar signalRestart
      result <-
        inLockstep conn (Proxy @RPC1) numCalls (TerminateAfter 1) $ \_results getFinal ->
          getFinal

      let expectedResult = [replicate numCalls $ StepOk 1]
      assertEqual "" expectedResult result

      -- Do not leave the server process hanging around
      killRestarted

{-------------------------------------------------------------------------------
  Auxiliary: echo handler
-------------------------------------------------------------------------------}

-- | Echos any input
echoHandler ::
     TrivialRpc rpc
  => Maybe (IORef Int)
  -> Server.Call rpc -> IO ()
echoHandler disconnectCounter call =
    trackDisconnects disconnectCounter $ loop
  where
    loop :: IO ()
    loop = do
        inp <- Binary.recvInput @Word64 call
        case inp of
          StreamElem n   -> Binary.sendNextOutput @Word64 call n >> loop
          FinalElem  n _ -> Binary.sendFinalOutput @Word64 call (n, NoMetadata)
          NoMoreElems  _ -> Server.sendTrailers call NoMetadata

    trackDisconnects :: Maybe (IORef Int) -> IO () -> IO ()
    trackDisconnects Nothing        = id
    trackDisconnects (Just counter) =
        handle $ \(_e :: Server.ClientDisconnected) ->
          atomicModifyIORef' counter $ \n -> (n + 1, ())

{-------------------------------------------------------------------------------
  Bunch of clients all executing in lockstep
-------------------------------------------------------------------------------}

data NumSteps = TerminateAfter Int | NeverTerminate

data Results = Results {
      -- | Results for the current step
      resultsCurr :: TVar [StepResult]

      -- | Number of the current step
    , resultsStep :: Int

      -- | Previous results (in reverse order)
    , resultsHist :: [[StepResult]]
    }

data StepResult = StepOk Int | StepFailed SomeException
  deriving stock (Show)

stepFailed :: StepResult -> Bool
stepFailed StepOk{}     = False
stepFailed StepFailed{} = True

instance Eq StepResult where
  StepOk     i == StepOk     i' = i == i'
  StepFailed _ == StepFailed _  = True -- the exception is merely for debugging
  StepOk     _ == StepFailed _  = False
  StepFailed _ == StepOk     _  = False

initResults :: IO (TVar Results)
initResults = do
    resultsCurr <- newTVarIO []
    newTVarIO Results{
        resultsCurr
      , resultsStep = 1
      , resultsHist = []
      }

-- | Keep collecting results (never terminates)
collectResults :: Int -> TVar Results -> IO a
collectResults numClients results =
    forever $
      atomically $ do
        Results{resultsCurr, resultsStep, resultsHist} <- readTVar results
        current <- readTVar resultsCurr
        if length current < numClients
          then retry
          else do
            current' <- newTVar []
            writeTVar results Results{
                resultsCurr = current'
              , resultsStep = succ resultsStep
              , resultsHist = current : resultsHist
              }

-- | Get the 'TVar' for the specified step, blocking until that step is reached
--
-- This is executed by each client on each step. As a result, we can assume that
-- the required step can never be /before/ the current step (because all clients
-- must deliver their result for the current step before the step advances).
waitForStep :: TVar Results -> Int -> IO (TVar [StepResult])
waitForStep results step = atomically $ do
    Results{resultsCurr, resultsStep} <- readTVar results
    if resultsStep < step
      then retry
      else return resultsCurr

-- | Wait until a history of at least the specified length is ready
waitForHistoryOfMinLen :: TVar Results -> Int -> IO [[StepResult]]
waitForHistoryOfMinLen results numSteps = atomically $ do
    Results{resultsHist} <- readTVar results
    if length resultsHist < numSteps
      then retry
      else return resultsHist

inLockstep :: forall rpc a.
     TrivialRpc rpc
  => Client.Connection -- ^ Server to connect to
  -> Proxy rpc         -- ^ Method to call
  -> Int               -- ^ Number of clients
  -> NumSteps          -- ^ How many steps each client should take
  -> (TVar Results -> IO [[StepResult]] -> IO a)
     -- ^ Monitor the results
     --
     -- This is also passed a function to get the /final/ results, after all
     -- clients have terminated. If some clients never terminate, this function
     -- will block indefinitely.
  -> IO a
inLockstep conn rpc numClients numSteps monitor = do
    results <- initResults
    withAsync (collectResults numClients results) $ \_ ->
      withAsync (runClients results) $ \clients ->
        monitor results (wait clients)
  where
    runClients :: TVar Results -> IO [[StepResult]]
    runClients results = do
        replicateConcurrently_ numClients $
          Client.withRPC conn def rpc (client results)
        resultsHist <$> readTVarIO results

    client :: TVar Results -> Client.Call rpc -> IO ()
    client results call = loop 1
      where
        loop :: Int -> IO ()
        loop n = do
            current <- waitForStep results n
            handle (recordException current) $
              case numSteps of
                TerminateAfter n' | n == n' -> do
                  Binary.sendFinalInput call n
                  (resp, NoMetadata) <- Binary.recvFinalOutput call
                  atomically $ modifyTVar current (StepOk resp:)
                _otherwise -> do
                  Binary.sendNextInput call n
                  resp <- Binary.recvNextOutput call
                  atomically $ modifyTVar current (StepOk resp:)
                  loop (succ n)

        recordException :: TVar [StepResult] -> SomeException -> IO ()
        recordException current e =
            atomically $ modifyTVar current (StepFailed e:)

{-------------------------------------------------------------------------------
  Auxiliary: trivial RPCs

  We want two distinct handler so we have two trivial RPCs.
-------------------------------------------------------------------------------}

type TrivialRpc rpc = (
    SupportsClientRpc        rpc
  , Input                    rpc ~ Lazy.ByteString
  , Output                   rpc ~ Lazy.ByteString
  , RequestMetadata          rpc ~ NoMetadata
  , ResponseInitialMetadata  rpc ~ NoMetadata
  , ResponseTrailingMetadata rpc ~ NoMetadata
  )

type RPC1 = Trivial' "rpc1"
type RPC2 = Trivial' "rpc2"

