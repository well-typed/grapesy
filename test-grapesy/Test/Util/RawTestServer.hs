module Test.Util.RawTestServer where


import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Builder qualified as BS.Builder
import Data.ByteString.Char8 qualified as BS.Strict.Char8
import Data.ByteString.UTF8 qualified as BS.Strict.UTF8
import Data.String (fromString)
import Data.Text qualified as Text
import Network.HTTP2.Server qualified as HTTP2
import Network.Run.TCP qualified as NetworkRun
import Network.Socket

import Network.GRPC.Client qualified as Client
import Network.HTTP.Types qualified as HTTP
import Network.GRPC.Common

{-------------------------------------------------------------------------------
  Test server

  This allows us to simulate broken /servers/.
-------------------------------------------------------------------------------}

-- | Low-level test server
--
-- We bypass the entire grapesy machinery for constructing the server, for added
-- flexibility. This allows us to mock broken deployments or run the server in
-- another thread that we throw asynchronous exceptions to, for example.
--
-- The grapesy client can auto reconnect when the server is not (yet) up and
-- running, but to keep things simple, and since the server anyway runs in the
-- same process, we just signal when the server is ready. This also allows us to
-- avoid binding to a specific port in the tests (which might already be in use
-- on the machine running the tests, leading to spurious test failures).
rawTestServer :: MVar PortNumber -> HTTP2.Server -> IO ()
rawTestServer serverPort server = do
    addr <- NetworkRun.resolve Stream (Just "127.0.0.1") "0" [AI_PASSIVE]
    bracket (NetworkRun.openTCPServerSocket addr) close $ \listenSock -> do
      addr' <- getSocketName listenSock
      port  <- case addr' of
                 SockAddrInet  port   _host   -> return port
                 SockAddrInet6 port _ _host _ -> return port
                 SockAddrUnix{} -> error "rawTestServer: unexpected unix socket"
      putMVar serverPort port
      NetworkRun.runTCPServerWithSocket listenSock $ \clientSock ->
        bracket (HTTP2.allocSimpleConfig clientSock 4096)
                HTTP2.freeSimpleConfig $ \config ->
          HTTP2.run HTTP2.defaultServerConfig config server

-- | Run the server and apply the continuation to an 'Client.Address' holding
-- the running server's host and port.
withTestServer :: HTTP2.Server -> (Client.Address -> IO a) -> IO a
withTestServer server k = do
    serverPort <- newEmptyMVar
    withAsync (rawTestServer serverPort server) $ \_serverThread -> do
      port <- readMVar serverPort
      let addr :: Client.Address
          addr = Client.Address {
                addressHost      = "127.0.0.1"
              , addressPort      = port
              , addressAuthority = Nothing
              }
      k addr

-- | Server that responds with the given 'Response', independent of the request
respondWith :: Response -> (Client.Address -> IO a) -> IO a
respondWith response = withTestServer $ \_req _aux respond ->
    respond (toHTTP2Response response) []

data Response = Response {
      responseStatus   :: HTTP.Status
    , responseHeaders  :: [HTTP.Header]
    , responseBody     :: Strict.ByteString
    , responseTrailers :: [HTTP.Header]
    }

instance Default Response where
  def = Response {
        responseStatus   = HTTP.ok200
      , responseHeaders  = [ asciiHeader "content-type" "application/grpc" ]
      , responseBody     = BS.Strict.empty
      , responseTrailers = [ asciiHeader "grpc-status" "0" ]
      }

toHTTP2Response :: Response -> HTTP2.Response
toHTTP2Response response =
    flip HTTP2.setResponseTrailersMaker trailersMaker $
      HTTP2.responseBuilder
        (responseStatus  response)
        (responseHeaders response)
        (BS.Builder.byteString $ responseBody response)
  where
    trailersMaker :: HTTP2.TrailersMaker
    trailersMaker Nothing  = return $ HTTP2.Trailers (responseTrailers response)
    trailersMaker (Just _) = return $ HTTP2.NextTrailersMaker trailersMaker

-- | Header with ASCII value
--
-- (Header /names/ are always ASCII.)
asciiHeader :: String -> String -> HTTP.Header
asciiHeader name value = (fromString name, BS.Strict.Char8.pack value)

-- | Header with UTF-8 encoded value
utf8Header :: String -> String -> HTTP.Header
utf8Header name value = (fromString name, BS.Strict.UTF8.fromString value)

grpcMessageContains :: GrpcException -> String -> Bool
grpcMessageContains GrpcException{grpcErrorMessage} str =
    case grpcErrorMessage of
      Just msg -> Text.pack str `Text.isInfixOf` msg
      Nothing  -> False
