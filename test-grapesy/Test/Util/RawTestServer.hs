module Test.Util.RawTestServer
  ( -- * Raw test server
    respondWith

    -- * Abstract response type
  , Response(..)
  , asciiHeader
  , utf8Header
  ) where

import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Builder qualified as BS.Builder
import Data.ByteString.Char8 qualified as BS.Strict.Char8
import Data.ByteString.UTF8 qualified as BS.Strict.UTF8
import Data.String (fromString)
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Server.Run
import Network.HTTP.Types qualified as HTTP

{-------------------------------------------------------------------------------
  Raw test server

  This allows us to simulate broken /servers/.
-------------------------------------------------------------------------------}

-- | Run the server and apply the continuation to an 'Client.Address' holding
-- the running server's host and port.
withTestServer :: HTTP2.Server -> (Client.Address -> IO a) -> IO a
withTestServer server k = do
    let serverConfig =
          ServerConfig {
            serverInsecure = Just $ InsecureConfig {
                insecureHost = Just "127.0.0.1"
              , insecurePort = 0
              }
            , serverSecure = Nothing
            }
    forkServer def serverConfig server $ \runningServer -> do
      port <- getServerPort runningServer
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
