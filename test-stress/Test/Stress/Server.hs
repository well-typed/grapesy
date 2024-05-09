module Test.Stress.Server
  ( server
  ) where

import Control.Exception
import Control.Monad
import Data.ByteString.Lazy.Char8 qualified as BS.Char8
import Data.IORef

import Network.GRPC.Common
import Network.GRPC.Server
import Network.GRPC.Server.Run
import Proto.API.Trivial

import Test.Stress.Common
import System.Exit (exitFailure)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

server :: Bool -> ServerConfig -> IO ()
server v config = handle swallowInterruptOrKilled $ do
    idRef <- newIORef "unknown"
    s <- mkGrpcServer def (handlers v idRef)
    forkServer def config s $ \runningServer -> do
      p <- getServerPort runningServer
      writeIORef idRef $ show p
      say v $ "server running on port " ++ show p
      waitServer runningServer
  where
    swallowInterruptOrKilled :: SomeException -> IO ()
    swallowInterruptOrKilled e
        | Just UserInterrupt <- asyncExceptionFromException e
        = say v "server received user interrupt, exiting gracefully"
        | Just ThreadKilled <- asyncExceptionFromException e
        = say v "server thread killed, exiting gracefully"
        | otherwise
        = do
          putStrLn $ "got unexpected server exception: " ++ show e
          exitFailure

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

handlers :: Bool -> IORef String -> [SomeRpcHandler IO]
handlers v idRef = [
      someRpcHandler @(Trivial' "non-streaming") $
        mkRpcHandler $ clientDisconnectOkay . nonStreaming
    , someRpcHandler @(Trivial' "server-streaming") $
        mkRpcHandler $ clientDisconnectOkay . serverStreaming
    , someRpcHandler @(Trivial' "client-streaming") $
        mkRpcHandler $ clientDisconnectOkay . clientStreaming
    , someRpcHandler @(Trivial' "bidi-streaming") $
        mkRpcHandler $ clientDisconnectOkay . bidiStreaming
    ]
  where
    -- Single message from client, single message from server
    nonStreaming :: Call (Trivial' "non-streaming") -> IO ()
    nonStreaming call = do
        say' "handling non-streaming call"
        msg <- recvFinalInput call
        sendFinalOutput call $ (msg, NoMetadata)
        say' "sent final output for non-streaming call"

    -- Client sends message containing number N, then client streams N messages
    -- to server
    clientStreaming :: Call (Trivial' "client-streaming") -> IO ()
    clientStreaming call = do
        say' "handling client streaming call"
        inp <- read @Int . BS.Char8.unpack <$> recvNextInput call
        say' $ "receiving " ++ show inp ++ " messages"
        forM_ [1 .. inp-1] $ \_ -> void $ recvNextInput call
        msg <- recvFinalInput call
        sendFinalOutput call (msg, NoMetadata)
        say' $ "sent final output for client streaming call"

    -- Client sends message containing number N, then server streams N messages
    -- to client
    serverStreaming :: Call (Trivial' "server-streaming") -> IO ()
    serverStreaming call = do
        say' "handling server streaming call"
        inp <- read @Int . BS.Char8.unpack <$> recvNextInput call
        say' $ "sending " ++ show inp ++ " messages"
        msg <- randomMsg
        forM_ [1 .. inp-1] $ \_ -> sendNextOutput call msg
        sendFinalOutput call (msg, NoMetadata)
        say' $ "sent final output for server streaming call"

    -- Client sends message containing number N, then client and server send N*2
    -- total messages back and forth.
    bidiStreaming :: Call (Trivial' "bidi-streaming") -> IO ()
    bidiStreaming call = do
        say' "handling bidi streaming call"
        inp <- read @Int . BS.Char8.unpack <$> recvNextInput call
        say' $ "sending and receiving " ++ show inp ++ " messages"
        msg <- randomMsg
        forM_ [1 .. inp-1] $ \_ -> do
          void $ recvNextInput call
          sendNextOutput call msg
        void $ recvFinalInput call
        sendFinalOutput call (msg, NoMetadata)
        say' $ "sent and received final messages for bidi streaming call"

    clientDisconnectOkay :: IO () -> IO ()
    clientDisconnectOkay =
        handle $ \case
          e | Just ClientDisconnected{} <- fromException e -> do
            say' "client disconnected"
            | otherwise ->
            throwIO e

    say' :: String -> IO ()
    say' msg = do
        sid <- readIORef idRef
        say v $ "(server " ++ sid ++ ") " ++ msg
