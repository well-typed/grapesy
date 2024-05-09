module Test.Stress.Client
  ( client
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy.Char8 qualified as BS.Char8
import GHC.IO.Exception
import Network.HTTP2.Client (HTTP2Error(..))
import Network.Socket
import Network.TLS (TLSException(..))

import Network.GRPC.Client hiding (Call)
import Network.GRPC.Common
import Network.GRPC.Common.Compression (Compression)
import Network.GRPC.Common.Compression qualified as Compr
import Proto.API.Trivial

import Test.Stress.Cmdline
import Test.Stress.Common

-------------------------------------------------------------------------------
-- Top-level
-------------------------------------------------------------------------------

client ::
     Bool
  -> Maybe ServerValidation
  -> PortNumber
  -> Compression
  -> [Connect]
  -> IO ()
client v mServerValidation serverPort compr =
    mapM_ $ runConnect v mServerValidation serverPort compr

runConnect ::
     Bool
  -> Maybe ServerValidation
  -> PortNumber
  -> Compression
  -> Connect
  -> IO ()
runConnect v mServerValidation serverPort compr Connect{..} = do
    say' v serverPort $
      "running calls " ++ show connectCalls ++
        case connectExec of
          Sequential -> " in sequence"
          Concurrent -> " concurrently"
    mapF (runCalls v mServerValidation serverPort compr callNum) $
      zip [1..] $ replicate connectNum connectCalls
  where
    mapF =
      case connectExec of
        Sequential -> mapM_
        Concurrent -> mapConcurrently_

runCalls ::
     Bool
  -> Maybe ServerValidation
  -> PortNumber
  -> Compression
  -> Int
  -> (Int, [Call])
  -> IO ()
runCalls v mServerValidation serverPort compr callNum (connNum, calls) = do
    say' v serverPort msg
    let connParams = def {
            connCompression = Compr.insist compr
          , connHTTP2Settings = def {
                http2TcpAbortiveClose = True
              }
          , connReconnectPolicy =
              exponentialBackoff
                (\d -> do
                  say' v serverPort $ "Reconnecting after " ++ show d ++ "μs"
                  threadDelay d
                )
                1
                (0.1, 0.1)
                maxBound

          }
    allowCertainFailures $
      withConnection connParams server $ \conn ->
        replicateM_ callNum $ mapM_ (runCall v serverPort conn) calls
  where
    addr :: Address
    addr = Address {
          addressHost      = "127.0.0.1"
        , addressPort      = serverPort
        , addressAuthority = Nothing
        }

    server :: Server
    server =
        case mServerValidation of
          Just serverValidation ->
            ServerSecure serverValidation SslKeyLogNone addr
          Nothing ->
            ServerInsecure addr

    allowCertainFailures :: IO () -> IO ()
    allowCertainFailures =
      handle $ \case
        e | Just ServerDisconnected{} <- fromException e ->
          say' v serverPort $ "server disconnected: " ++ show e
          | Just IOError{} <- fromException e ->
          say' v serverPort "failed to connect"
          | Just ConnectionIsTimeout <- fromException e ->
          say' v serverPort "got ConnectionIsTimeout"
          | Just BadThingHappen{} <- fromException e ->
          say' v serverPort "got BadThingHappen"
          | Just HandshakeFailed{} <- fromException e ->
          say' v serverPort "got HandshakeFailed"
          | Just BlockedIndefinitelyOnSTM{} <- fromException e ->
          say' v serverPort "got BlockedIndefinitelyOnSTM"
          | otherwise -> do
          say' v serverPort $ "got exception: " ++ displayException e
          throwIO e

    msg :: String
    msg =
           "opening connection " ++ show connNum ++ " to "
        ++ case mServerValidation of
             Just _ -> "secure "
             Nothing -> "insecure "
        ++ "server at port " ++ show serverPort ++ " with compression "
        ++ show (Compr.compressionId compr)

runCall :: Bool -> PortNumber -> Connection -> Call -> IO ()
runCall v p conn =
    \case
      NonStreaming ->
        nonStreaming v p conn
      ClientStreaming n ->
        clientStreaming v p conn n
      ServerStreaming n ->
        serverStreaming v p conn n
      BiDiStreaming n ->
        bidiStreaming v p conn n

-------------------------------------------------------------------------------
-- Specific RPCs
-------------------------------------------------------------------------------

-- | One non-streaming, round-trip call
nonStreaming :: Bool -> PortNumber -> Connection -> IO ()
nonStreaming v p conn = do
    say' v p "initiating non-streaming call"
    withRPC conn def (Proxy @(Trivial' "non-streaming")) $ \call -> do
      sendFinalInput call =<< randomMsg
      void $ recvFinalOutput call
      say' v p "received final output for non-streaming call"

-- | Client streaming
--
-- Client sends the server @N@, followed by @N@ messages.
clientStreaming :: Bool -> PortNumber -> Connection -> Int -> IO ()
clientStreaming v p conn n = do
    say' v p "initiating client streaming call"
    withRPC conn def (Proxy @(Trivial' "client-streaming")) $ \call -> do
      say' v p $ "sending " ++ show n ++ " messages"
      sendNextInput call $ BS.Char8.pack (show n)
      msg <- randomMsg
      forM_ [1 .. n-1] $ \_ ->
        void $ sendNextInput call msg
      sendFinalInput call msg
      void $ recvFinalOutput call
      say' v p "received final output for client streaming call"

-- | Server streaming
--
-- Client sends the server @N@, then receives @N@ messages from server.
serverStreaming :: Bool -> PortNumber -> Connection -> Int -> IO ()
serverStreaming v p conn n = do
    say' v p "initiating server streaming call"
    withRPC conn def (Proxy @(Trivial' "server-streaming")) $ \call -> do
      say' v p $ "receiving " ++ show n ++ " messages"
      sendFinalInput call $ BS.Char8.pack (show n)
      forM_ [1 .. n-1] $ \_ -> void $ recvNextOutput call
      void $ recvFinalOutput call
      say' v p "received final output for server streaming call"

-- | Bidirectional streaming
--
-- Client sends the server @N@, then alternates sending and receiving @N*2@
-- total messages.
bidiStreaming :: Bool -> PortNumber -> Connection -> Int -> IO ()
bidiStreaming v p conn n = do
    say' v p "initiating bidi streaming call"
    withRPC conn def (Proxy @(Trivial' "bidi-streaming")) $ \call -> do
      say' v p $ "sending and receiving " ++ show n ++ " messages"
      sendNextInput call $ BS.Char8.pack (show n)
      msg <- randomMsg
      forM_ [1 .. n-1] $ \_ -> do
        sendNextInput call msg
        void $ recvNextOutput call
      sendFinalInput call msg
      void $ recvFinalOutput call
      say' v p "sent and received final messages for bidi streaming call"

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

say' :: Bool -> PortNumber -> String -> IO ()
say' v p msg =
  say v $ "(client " ++ show p ++ ") " ++ msg
