{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Client (
    -- * Connecting to the server
    withConnection
    -- * Open connection
  , Connection -- opaque
  , withRPC
  , startRPC
  , stopRPC
    -- * Open request
    --
    -- $openRequest
  , OpenCall -- opaque
  , IsFinal(..)
  , Trailers(..)
  , sendInput
  , recvOutput
    -- * Logging
  , LogMsg(..)
  , LogRPC(..)
  ) where

import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Tracer
import Network.HPACK qualified as HPACK
import Network.HTTP2.Client qualified as Client
import Network.Run.TCP (runTCPClient)

import Network.GRPC.Client.OpenCall
import Network.GRPC.Spec
import Network.GRPC.Spec.HTTP2 qualified as HTTP2
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Thread
import Control.Monad.IO.Class
import GHC.Stack

{-------------------------------------------------------------------------------
  Connection API
-------------------------------------------------------------------------------}

-- | Open connection
--
-- This type is kept abstract (opaque) in the user facing API.
data Connection = Connection {
      -- | Logging
      --
      -- Most applications will probably just set this to 'nullTracer' to
      -- disable logging.
      connTracer :: Tracer IO LogMsg

      -- | Send request
      --
      -- This is the function we get from @http2@.
    , connSendRequest :: forall a.
           Client.Request
        -> (Client.Response -> IO a)
        -> IO a
    }

-- | Start RPC call
--
-- This is a non-blocking call; the connection will be set up in a background
-- thread; if this takes time, then the first call to 'sendInput' or
-- 'recvOutput' will block, but the call to 'startRPC' itself will not block.
-- This non-blocking nature makes this safe to use in 'bracket' patterns.
--
-- This is a low-level API. Consider using 'withRPC' instead.
startRPC :: forall rpc.
     IsRPC rpc
  => Connection
  -> RequestMeta
  -> rpc
  -> IO (OpenCall rpc)
startRPC Connection{connTracer, connSendRequest} meta rpc =
    openCall (contramap (LogRPC rpc) connTracer) connSendRequest meta rpc

-- | Stop RPC call
--
-- This is a low-level API. Consider using 'withRPC' instead.
--
-- NOTE: When an 'OpenCall' is closed, it remembers the /reason/ why it was
-- closed. For example, if it is closed due to a network exception, then this
-- network exception is recorded as part of this reason. When the call is closed
-- due to call to 'stopRPC', we merely record that the call was closed due to
-- a call to 'stopRPC' (along with a callstack). /If/ the call to 'stopRPC'
-- /itself/ was due to an exception, this exception will not be recorded; if
-- that is undesirable, consider using 'withRPC' instead.
stopRPC :: HasCallStack => OpenCall rpc -> IO ()
stopRPC = flip closeCall Nothing

{-------------------------------------------------------------------------------
  Bracketing
-------------------------------------------------------------------------------}

-- | Scoped RPC call
withRPC :: forall m rpc a.
     (MonadMask m, MonadIO m, IsRPC rpc)
  => Connection
  -> RequestMeta
  -> rpc
  -> (OpenCall rpc -> m a)
  -> m a
withRPC conn meta rpc = fmap aux .
    generalBracket
      (liftIO $ startRPC conn meta rpc)
      (\call -> liftIO . \case
          ExitCaseSuccess   _ -> closeCall call $ Nothing
          ExitCaseException e -> closeCall call $ Just e
          ExitCaseAbort       -> closeCall call $ Just (toException Aborted)
      )
  where
    aux :: (a, ()) -> a
    aux = fst

-- | Exception corresponding to the 'ExitCaseAbort' case of 'ExitCase'
data Aborted = Aborted
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Open a new connection
-------------------------------------------------------------------------------}

withConnection :: forall a.
     Tracer IO LogMsg
  -> Scheme
  -> Authority
  -> (Connection -> IO a)
  -> IO a
withConnection connTracer scheme auth k =
    runTCPClient (authorityHost auth) (show $ authorityPort auth)  $ \sock ->
      bracket
          (Client.allocSimpleConfig sock bufferSize)
          Client.freeSimpleConfig $ \conf ->
        Client.run clientConfig conf $ \connSendRequest ->
          k Connection{connTracer, connSendRequest}
  where
    -- See docs of 'confBufferSize', but importantly: "this value is announced
    -- via SETTINGS_MAX_FRAME_SIZE to the peer."
    --
    -- Value of 4kB is taken from the example code.
    bufferSize :: HPACK.BufferSize
    bufferSize = 4096

    clientConfig :: Client.ClientConfig
    clientConfig = Client.ClientConfig {
          scheme = case scheme of
                     Http  -> "http"
                     Https -> "https"

          -- The example code does not include the port number here; the RFC
          -- lists it as optional
          -- <https://www.rfc-editor.org/rfc/rfc3986#section-3.2>
        , authority = HTTP2.authority auth

          -- Docs describe this as "How many pushed responses are contained in
          -- the cache". I don't think I really know what this means. Value of
          -- 20 is from the example code.
        , cacheLimit = 20
        }

{-------------------------------------------------------------------------------
  'OpenCall' API
-------------------------------------------------------------------------------}

-- $openRequest
--
-- 'OpenCall' denotes a previously opened request (see 'withRequest').
--
-- == Specialization to Protobuf
--
-- This is a general implementation of the
-- [gRPC specification](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md).
-- As such, these functions do not provide explicit support for the common
-- communication patterns of non-streaming, server-side streaming, client-side
-- streaming, or bidirectional streaming. These are not part of the gRPC
-- standard, but are part of its Protobuf instantiation; see
-- "Network.GRPC.Protobuf".
--
-- == Final messages and trailers
--
-- There are two asymmetries between 'sendInput' and 'recvOutput'; they are the
-- direct consequence of properties of the
-- [gRPC specification](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md),
-- specifically how it deals with 'Trailers'.
--
-- *   'sendInput' takes a simple flag 'IsFinal' as argument telling it whether
--     a message is 'Final' or 'NotFinal', whereas 'recvOutput' returns
--     'Trailers' after the final message is received, allowing the server to
--     return some additional information (for example, a CRC checksum of the
--     data sent).
--
--     This is because the gRPC does not allow for request trailers.
--
-- *   'sendInput' takes /both/ the 'IsFinal' flag /and/ an 'Input', whereas
--     'recvOutput' returns /either/ 'Trailers' (retroactively indicating that
--     the previously returned 'Output' was the last) /or/ an 'Output'.
--
--     The reason here is a bit more subtle. Suppose we are doing a @grpc+proto@
--     non-streaming RPC call. The input message from the client to the server
--     will be sent over one or more HTTP2 @DATA@ frames (chunks of the input).
--     The server will expect the last of those frames to be marked as
--     @END_STREAM@. The HTTP2 specification /does/ allow sending an separate
--     empty @DATA@ frame with the @END_STREAM@ flag set to indicate no further
--     data is coming, but not all gRPC servers will wait for this, and might
--     either think that the client is broken and disconnect, or might send the
--     client a @RST_STREAM@ frame to force it to close the stream. To avoid
--     problems, therefore, it is better to mark the final @DATA@ frame as
--     @END_STREAM@; in order to be able to do that, 'sendInput' needs to know
--     whether an input is the final one.
--
--     For 'recvOutput' the situation is different. Now we /do/ expect HTTP
--     trailers, which means that we cannot tell from @DATA@ frames alone
--     if we have received the last message (it will be the frame containing the
--     /trailers/ that is marked as @END_STREAM@, with no indication on the data
--     frame just before it that it was the last one). We cannot wait for the
--     next frame to come in, because that would be a blocking call (we might
--     have to wait for the next TCP packet), and if the output was /not/ the
--     last one, we would unnecessarily delay making the output we already
--     received available to the client code.
--
--     Of course, the /client code/ may know whether it any given message was
--     the last; in the example above of a non-streaming @grpc+proto@ RPC call,
--     we only expect a single output. In this case the client can (and should)
--     call 'recvOutput' again to wait for the trailers (which, amongst other
--     things, will include the 'grpcStatus').
--
-- If you are using the specialized functions from "Network.GRPC.Protobuf",
-- you do not need to worry about any of this.

data RpcException =
    -- | Call to 'sendInput' after the 'Final' 'Output' was sent
    SendAfterFinal

    -- | Call to 'recvOutput' after receiving 'Trailers'
  | RecvAfterTrailers
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Send an input to the peer
--
-- This lives in @STM@ for improved composability. For example, if the peer is
-- currently busy then 'sendInput' will block, but you can then use 'orElse' to
-- provide an alternative codepath.
--
-- Calling 'sendInput' after sending the final message will result in a
-- 'SendAfterFinal' exception.
sendInput ::
     OpenCall rpc
  -> IsFinal
  -> Maybe (Input rpc)
  -> STM ()
sendInput OpenCall{callOutbound} isFinal input =
    getThreadInterface callOutbound >>= \case
      Just q  -> putTMVar q (isFinal, input)
      Nothing -> throwSTM SendAfterFinal

-- | Receive an output from the peer
--
-- This lives in @STM@ for improved compositionality. For example, you can wait
-- on multiple clients and see which one responds first.
--
-- Though the types cannot guarantee it, you will receive 'Trailers' once, after
-- the final 'Output'. Calling 'recvOutput' after receiving 'Trailers' will
-- result in a 'RecvAfterTrailers' exception.
recvOutput :: OpenCall rpc -> STM (Either Trailers (Output rpc))
recvOutput OpenCall{callInbound} =
    getThreadInterface callInbound >>= \case
      Just q  -> takeTMVar q
      Nothing -> throwSTM RecvAfterTrailers

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data LogMsg =
    forall rpc. IsRPC rpc => LogRPC rpc (LogRPC rpc)

deriving instance Show LogMsg


