module Network.GRPC.Util.HTTP2.Stream (
    -- * Streams
    OutputStream(..)
  , InputStream(..)
    -- * Server API
  , serverOutputStream
  , serverInputStream
    -- ** Client API
  , clientInputStream
  , clientOutputStream
  ) where

import Control.Monad
import Data.Binary.Builder (Builder)
import Data.ByteString qualified as Strict
import Data.IORef
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Client qualified as Client
import Network.HTTP2.Server qualified as Server

import Network.GRPC.Util.HTTP2 (fromHeaderTable)

{-------------------------------------------------------------------------------
  Streams
-------------------------------------------------------------------------------}

data OutputStream = OutputStream {
      -- | Write a chunk to the stream
      writeChunk :: Builder -> IO ()

      -- | Flush the stream (send frames to the peer)
    , flush :: IO ()

      -- | Indicate that there will be no more data
    , closeOutputStream :: IO ()
    }

data InputStream = InputStream {
      getChunk    :: IO Strict.ByteString
    , getTrailers :: IO [HTTP.Header]
    }

{-------------------------------------------------------------------------------
  Server API
-------------------------------------------------------------------------------}

serverInputStream :: Server.Request -> IO InputStream
serverInputStream req = do
    return InputStream {
        getChunk    = Server.getRequestBodyChunk req
      , getTrailers = maybe [] fromHeaderTable <$>
                        Server.getRequestTrailers req
      }

-- | Create output stream
--
-- == Note on the use of Trailers-Only in non-error cases
--
-- If the stream is closed without writing anything, the situation is similar to
-- the gRPC @Trailers-Only@ case, except that we have already sent the initial
-- set of headers. In this case, http2 will (reasonably enough) create an empty
-- DATA frame, and then another HEADERS frame for the trailers. This is conform
-- the gRPC specification, which mandates:
--
-- > Most responses are expected to have both headers and trailers but
-- > Trailers-Only is permitted for calls that produce an immediate error.
--
-- If we compare this to the example server, however, we see that all headers
-- might delivered in a /single/ frame (i.e., the proper Trailers-Only case). An
-- example is @RouteGuide.listFeatures@: when there /are/ no features in the
-- specified rectangle, the server will send no messages back to the client. The
-- example Python server will use the gRPC Trailers-Only case here (and so we
-- must be able to deal with that in our client implementation).
--
-- Fortunately, it seems that if we do /not/ make use of the Trailers-Only case
-- (our current server implementation) then the example Python client will still
-- interpret the results correctly, so we don't have to follow suit. Indeed,
-- technically what the Python server does is not conform spec, as mentioned
-- above.
--
-- If we /do/ want to do what the example Python server does, it's a bit tricky,
-- as it leads to two requirements that seem in opposition:
--
-- 1. We should wait until we create the request (and its headers) until we know
--    for sure some data is written. If no data is written, we are in the
--    @Trailers-Only@ case, and we should generate /different/ headers (this
--    probably means we should introduce Trailers-Only data types after all; see
--    discussion of 'parseTrailers').
-- 2. We should be able to send the response headers /before/ the first message
--    (e.g. @wait_for_ready_with_client_timeout_example_client.py@ tests that it
--    receives the initial metadata before the first response).
--
-- Perhaps a way out here is that we can create the response as soon as we have
-- a /promise/ of the first message, even if that first message may still need
-- time to compute.
serverOutputStream :: (Builder -> IO ()) -> IO () -> IO OutputStream
serverOutputStream writeChunk flush = do
    return OutputStream {
        writeChunk
      , flush
      , closeOutputStream = return ()
      }

{-------------------------------------------------------------------------------
  Client API
-------------------------------------------------------------------------------}

clientInputStream :: Client.Response -> IO InputStream
clientInputStream resp = do
    return InputStream {
        getChunk    = Client.getResponseBodyChunk resp
      , getTrailers = maybe [] fromHeaderTable <$>
                        Client.getResponseTrailers resp
      }

clientOutputStream :: (Builder -> IO ()) -> IO () -> IO OutputStream
clientOutputStream writeChunk flush = do
    wroteSomethingRef <- newIORef False
    return OutputStream {
        writeChunk = \chunk -> do
          atomicModifyIORef wroteSomethingRef $ \_ -> (True, ())
          writeChunk chunk
      , flush

        -- The http2 client implementation has an explicit check that means the
        -- response is not sent until /something/ has been written
        -- <https://github.com/kazu-yamamoto/http2/commit/a76cdf3>; to
        -- workaround this limitation we write an empty chunk. This results in
        -- an empty data frame in the stream, but this does not matter: gRPC
        -- does not support request headers, which means that, unlike in the
        -- server, we do not need to give the empty case special treatment.
        -- TODO: Nonetheless, it might be better to patch http2 to avoid this
        -- workaround.
      , closeOutputStream = do
          wroteSomething <- readIORef wroteSomethingRef
          unless wroteSomething $ writeChunk mempty
      }


