{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Client (
    -- * Connecting to the server
    Connection -- opaque
  , Server(..)
  , ConnParams(..)
  , withConnection

    -- ** Connection parameters
  , Scheme(..)
  , Authority(..)

    -- ** Secure connection (TLS)
  , ServerValidation(..)
  , Util.TLS.CertificateStoreSpec
  , Util.TLS.certStoreFromSystem
  , Util.TLS.certStoreFromCerts
  , Util.TLS.certStoreFromPath

    -- * Make RPCs
  , Call -- opaque
  , withRPC

    -- ** Call parameters
  , CallParams(..)

    -- *** Timeouts
  , Timeout(..)
  , TimeoutValue(TimeoutValue, getTimeoutValue)
  , TimeoutUnit(..)
  , timeoutToMicro

    -- * Ongoing calls
    --
    -- $openRequest
  , sendInput
  , recvOutput
  , recvResponseMetadata

    -- ** Protocol specific wrappers
  , sendFinalInput
  , sendAllInputs
  , recvFinalOutput
  , recvAllOutputs

    -- ** Low-level API
  , sendInputSTM
  , recvOutputSTM
  , startRPC
  , closeRPC

    -- * Common serialization formats
  , Protobuf

    -- * Debugging
  , ClientDebugMsg(..)
  ) where

import Network.GRPC.Client.Call
import Network.GRPC.Client.Connection
import Network.GRPC.Spec
import Network.GRPC.Util.TLS qualified as Util.TLS

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

