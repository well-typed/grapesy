module Test.Disconnect.Keepalive (
    test_keepAlivePreventsIdleClose
  , test_idleTimeoutWithoutKeepAliveDisconnects
  ) where

import Control.Concurrent
import Control.Exception (SomeException, fromException, try)
import Data.Proxy
import Data.Word
import Network.HTTP2.Client (HTTP2Error(..))
import Network.Socket (PortNumber)
import Test.HUnit

import Network.GRPC.Common
import Network.GRPC.Client        qualified as Client
import Network.GRPC.Client.Binary qualified as Client.Binary
import Network.GRPC.Server        qualified as Server

import Network.GRPC.Server.Run qualified as Grapesy

import Test.Disconnect.Echo.RPC
import Test.Disconnect.Echo.Server

{-------------------------------------------------------------------------------
  Test client idle timeout / keepalive ping

  'http2' closes a connection once idle for 'http2ClientIdleTimeout' (default
  30s); a keepalive ping ('http2ClientKeepAlivePingInterval') resets that
  timer even when no RPCs are in flight.

  We use a short idle timeout so the tests run quickly, and check both
  directions: a keepalive ping keeps an otherwise-silent connection alive
  past the timeout, and without one the connection really is closed (with
  @http2@'s own 'ConnectionIsTimeout') -- proving the first case isn't
  trivially true just because nothing ever closes idle connections.
-------------------------------------------------------------------------------}

-- | How long the client is configured to tolerate a silent connection
idleTimeout :: Int
idleTimeout = 500_000 -- 500ms

-- | Keepalive ping interval, comfortably below 'idleTimeout'
keepAlivePingInterval :: Int
keepAlivePingInterval = 100_000 -- 100ms

-- | How long the test stays silent before attempting another RPC
--
-- Comfortably past 'idleTimeout', so an unkept-alive connection is
-- guaranteed to have been closed by @http2@ by the time we check.
silence :: Int
silence = 2_000_000 -- 2s

test_keepAlivePreventsIdleClose :: Assertion
test_keepAlivePreventsIdleClose =
    withServer $ \serverAddr ->
      withKeepAlive (Just keepAlivePingInterval) serverAddr $ \conn -> do
        threadDelay silence
        result <- tryEcho conn
        assertBool ("expected successful echo, got " ++ show result) $
          case result of
            Right 42   -> True
            _otherwise -> False

test_idleTimeoutWithoutKeepAliveDisconnects :: Assertion
test_idleTimeoutWithoutKeepAliveDisconnects =
    withServer $ \serverAddr ->
      withKeepAlive Nothing serverAddr $ \conn -> do
        threadDelay silence
        result <- tryEcho conn
        assertBool ("expected ConnectionIsTimeout, got " ++ show result) $
          case result of
            Left e | Just ConnectionIsTimeout <- fromException e -> True
            _otherwise -> False

{-------------------------------------------------------------------------------
  Util
-------------------------------------------------------------------------------}

-- | Connect with 'idleTimeout' and the given keepalive ping interval
withKeepAlive ::
     Maybe Int -- ^ 'http2ClientKeepAlivePingInterval'
  -> Client.Server
  -> (Client.Connection -> IO a)
  -> IO a
withKeepAlive pingInterval = Client.withConnection connParams
  where
    connParams = def {
        Client.connHTTP2Settings = def {
            http2ClientIdleTimeout           = Just idleTimeout
          , http2ClientKeepAlivePingInterval  = pingInterval
          }
      }

-- | Run one echo round-trip, properly closing the stream
--
-- Leaving 'withRPC''s scope with the request stream still open is itself
-- reported as a client-side 'GrpcCancelled' -- indistinguishable from the
-- failure we're testing for -- so we close it ('sendEndOfInput') and
-- consume the trailers ourselves.
tryEcho :: Client.Connection -> IO (Either SomeException Word64)
tryEcho conn = try $
    Client.withRPC conn def (Proxy @Echo1) $ \call -> do
      Client.Binary.sendNextInput @Word64 call 42
      resp <- Client.Binary.recvNextOutput @Word64 call
      Client.sendEndOfInput call
      NoMetadata <- Client.recvTrailers call
      return resp

withServer :: (Client.Server -> IO a) -> IO a
withServer k = do
    let handler = Server.someRpcHandler $
                    Server.mkRpcHandler @Echo1 (handleEcho (Proxy @Echo1))
    server <- Server.mkGrpcServer def [handler]
    Grapesy.forkServer def serverConfig server $ \runningServer -> do
      serverPort <- Grapesy.getServerPort runningServer
      k $ mkServerAddress serverPort
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
