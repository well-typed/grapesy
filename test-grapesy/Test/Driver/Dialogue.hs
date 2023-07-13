module Test.Driver.Dialogue (
    execGlobalSteps
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Thread.Delay
import Control.Exception
import Control.Monad
import Data.Default
import Data.Proxy

import Network.GRPC.Client (Timeout, timeoutToMicro)
import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Client.Binary
import Network.GRPC.Common.Binary
import Network.GRPC.Common.CustomMetadata (CustomMetadata)
import Network.GRPC.Common.StreamElem (StreamElem)
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Server.Binary

import Test.Driver.ClientServer

{-------------------------------------------------------------------------------
  RPC
-------------------------------------------------------------------------------}

type TestRpc = BinaryRpc "binary" "test"

{-------------------------------------------------------------------------------
  Barrier

  These are concurrent tests, but it can be difficult to pinpoint bugs if the
  ordering of actions is not determistic. We therefore add specific "barrier"
  points, which allow to shrink tests towards more deterministic.
-------------------------------------------------------------------------------}

newtype Barrier = Barrier Int
  deriving stock (Show, Eq, Ord)

newBarrier :: IO (TVar Barrier)
newBarrier = newTVarIO (Barrier 0)

waitForBarrier :: TVar Barrier -> Barrier -> IO ()
waitForBarrier var target = atomically $ do
    actual <- readTVar var
    when (actual < target) retry

{-------------------------------------------------------------------------------
  Test failures
-------------------------------------------------------------------------------}

data TestFailure =
    -- | Thrown by the server when an unexpected new RPC is initiated
    UnexpectedRequest

    -- | Server received an unexpected value
  | ServerUnexpected ReceivedUnexpected

    -- | Client received an unexpected value
  | ClientUnexpected ReceivedUnexpected
  deriving stock (Show)
  deriving anyclass (Exception)

data ReceivedUnexpected = forall a. Show a => ReceivedUnexpected {
      expected :: a
    , received :: a
    }

deriving stock instance Show ReceivedUnexpected

{-------------------------------------------------------------------------------
  Single channel

  TODO: Exceptions (both GrpcException and general other exceptions).

  TODO: We should test that the Trailers-Only case gets triggered if no messages
  were exchanged before the exception (not sure this is observable without
  Wireshark..?).
-------------------------------------------------------------------------------}

data LocalStep =
    StartResponse [CustomMetadata]
  | LocalBarrier Barrier
  | ServerSleep Timeout
  | ClientToServer (StreamElem () Int)
  | ServerToClient (StreamElem [CustomMetadata] Int)
  deriving stock (Show)

type LocalSteps = [LocalStep]

{-------------------------------------------------------------------------------
  Many channels (birds-eye view)
-------------------------------------------------------------------------------}

data GlobalStep =
    Spawn (Maybe Timeout) [CustomMetadata] LocalSteps
  | GlobalBarrier Barrier
  deriving stock (Show)

type GlobalSteps = [GlobalStep]

{-------------------------------------------------------------------------------
  Client-side interpretation
-------------------------------------------------------------------------------}

clientGlobal :: TVar Barrier -> Client.Connection -> GlobalSteps -> IO ()
clientGlobal barrierVar conn =
    mapM_ go
  where
    go :: GlobalStep -> IO ()
    go (GlobalBarrier b) = do
        waitForBarrier barrierVar b
    go (Spawn timeout metadata localSteps) =
        void $ forkIO $ do
          call <- Client.startRPC conn params (Proxy @TestRpc)
          clientLocal barrierVar call localSteps
      where
        params :: Client.CallParams
        params = Client.CallParams {
              callTimeout         = timeout
            , callRequestMetadata = metadata
            }

clientLocal :: TVar Barrier -> Client.Call TestRpc -> LocalSteps -> IO ()
clientLocal barrierVar call localSteps = do
    mapM_ go localSteps
    Client.abortRPC call
  where
    go :: LocalStep -> IO ()
    go (LocalBarrier barrier) =
        waitForBarrier barrierVar barrier
    go (StartResponse expectedMetadata) = do
        receivedMetadata <- atomically $ Client.recvResponseMetadata call
        unless (receivedMetadata == expectedMetadata) $
          throwIO $ ClientUnexpected $ ReceivedUnexpected {
              expected = expectedMetadata
            , received = receivedMetadata
            }
    go (ServerSleep _) =
        -- Server delays are not (directly) observable in the client. The only
        -- things we should be able to observe is if the client-specified
        -- timeout is exceeded.
        return ()
    go (ClientToServer x) =
        Client.Binary.sendInput call x
    go (ServerToClient expectedElem) = do
        receivedElem <- Client.Binary.recvOutput call
        unless (receivedElem == expectedElem) $
          throwIO $ ClientUnexpected $ ReceivedUnexpected {
              expected = expectedElem
            , received = receivedElem
            }

{-------------------------------------------------------------------------------
  Server-side interpretation

  The server-side is slightly different, since the infrastructure spawns
  threads on our behalf (one for each incoming RPC).
-------------------------------------------------------------------------------}

serverGlobal :: TVar Barrier -> MVar GlobalSteps -> Server.Call TestRpc -> IO ()
serverGlobal barrierVar globalStepsVar call = do
    -- The client will only start new calls from a /single/ thread, so the order
    -- that these come in is determinstic. Here we peel off the next
    -- 'LocalSteps', pending barriers perhaps, whilst holding the lock, to
    -- ensure the same determinism server-side.
    mapM_ go =<< modifyMVar globalStepsVar getNextSteps
  where
    getNextSteps :: GlobalSteps -> IO (GlobalSteps, LocalSteps)
    getNextSteps [] =
        throwIO $ UnexpectedRequest
    getNextSteps (GlobalBarrier barrier : global') = do
        waitForBarrier barrierVar barrier
        getNextSteps global'
    getNextSteps (Spawn _timeout expectedMetadata localSteps : global') = do
        -- We don't care about the timeout the client sets; if the server takes
        -- too long (see 'ServerSleep'), the framework should kill the server
        -- and the /client/ will check that it gets the expected exception.
        receivedMetadata <- Server.getRequestMetadata call
        unless (receivedMetadata == expectedMetadata) $
          throwIO $ ServerUnexpected $ ReceivedUnexpected {
              expected = expectedMetadata
            , received = receivedMetadata
            }
        return (global', localSteps)

    go :: LocalStep -> IO ()
    go (LocalBarrier barrier) =
        waitForBarrier barrierVar barrier
    go (StartResponse metadata) = do
        Server.setResponseMetadata call metadata
        void $ Server.initiateResponse call
    go (ServerSleep timeout) = do
        delay $ timeoutToMicro timeout
    go (ClientToServer expectedElem) = do
        receivedElem <- Server.Binary.recvInput call
        unless (expectedElem == receivedElem) $
          throwIO $ ServerUnexpected $ ReceivedUnexpected {
              expected = expectedElem
            , received = receivedElem
            }
    go (ServerToClient x) = do
        Server.Binary.sendOutput call x

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

execGlobalSteps :: GlobalSteps -> IO ClientServerTest
execGlobalSteps steps = do
    barrierVar     <- newBarrier
    globalStepsVar <- newMVar steps
    return $ def {
        client = \conn -> clientGlobal barrierVar conn steps
      , server = [ Server.mkRpcHandler (Proxy @TestRpc) $
                     serverGlobal barrierVar globalStepsVar
                 ]
      }

