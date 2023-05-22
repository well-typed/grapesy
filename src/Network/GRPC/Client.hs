{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Client (
    -- * Connecting to the server
    Connection -- opaque
  , withConnection

    -- ** Connection parameters
  , ConnParams(..)
  , Scheme(..)
  , Authority(..)
  , defaultConnParams

    -- * Make RPCs
  , Call -- opaque
  , withRPC
  , startRPC
  , stopRPC

    -- ** Call parameters
  , CallParams(..)

    -- *** Timeouts
  , Timeout(..)
  , TimeoutValue(TimeoutValue, getTimeoutValue)
  , TimeoutUnit(..)

    -- * Ongoing calls
    --
    -- $openRequest
  , IsFinal(..)
  , Trailers(..)
  , sendInput
  , recvOutput
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import GHC.Stack

import Network.GRPC.Client.Call
import Network.GRPC.Client.Connection
import Network.GRPC.Client.Connection.Params
import Network.GRPC.Spec
import Network.GRPC.Spec.PseudoHeaders (Scheme(..), Authority(..))
import Network.GRPC.Spec.RPC

{-------------------------------------------------------------------------------
  Make RPCs
-------------------------------------------------------------------------------}

-- | Start RPC call
--
-- This is a non-blocking call; the connection will be set up in a background
-- thread; if this takes time, then the first call to 'sendInput' or
-- 'recvOutput' will block, but the call to 'startRPC' itself will not block.
-- This non-blocking nature makes this safe to use in 'bracket' patterns.
--
-- This is a low-level API. Consider using 'withRPC' instead.
startRPC :: IsRPC rpc => Connection -> CallParams -> rpc -> IO (Call rpc)
startRPC = openCall

-- | Stop RPC call
--
-- This is a low-level API. Consider using 'withRPC' instead.
--
-- NOTE: When an 'Call' is closed, it remembers the /reason/ why it was
-- closed. For example, if it is closed due to a network exception, then this
-- network exception is recorded as part of this reason. When the call is closed
-- due to call to 'stopRPC', we merely record that the call was closed due to
-- a call to 'stopRPC' (along with a callstack). /If/ the call to 'stopRPC'
-- /itself/ was due to an exception, this exception will not be recorded; if
-- that is undesirable, consider using 'withRPC' instead.
stopRPC :: HasCallStack => Call rpc -> IO ()
stopRPC = flip closeCall Nothing

-- | Scoped RPC call
--
-- May throw
--
-- * 'RpcImmediateError' when we fail to establish the call
-- * 'CallClosed' when attempting to send or receive data an a closed call.
withRPC :: forall m rpc a.
     (MonadMask m, MonadIO m, IsRPC rpc)
  => Connection -> CallParams -> rpc -> (Call rpc -> m a) -> m a
withRPC conn params rpc = fmap aux .
    generalBracket
      (liftIO $ startRPC conn params rpc)
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
  Ongoing calls
-------------------------------------------------------------------------------}

-- $openRequest
--
-- 'Call' denotes a previously opened request (see 'withRequest').
--
-- == Specialization to Protobuf
--
-- This is a general implementation of the
-- [gRPC specification](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md).
-- As such, these functions do not provide explicit support for the common
-- communication patterns of non-streaming, server-side streaming, client-side
-- streaming, or bidirectional streaming. These are not part of the gRPC
-- standard, but are part of its Protobuf instantiation; see
-- "Network.GRPC.Client.Protobuf".
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
--     things, will include the 'trailerGrpcStatus').
--
-- If you are using the specialized functions from
-- "Network.GRPC.Client.Protobuf" you do not need to worry about any of this.

