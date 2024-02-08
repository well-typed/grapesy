{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Client (
    -- * Connecting to the server
    Connection -- opaque
  , Server(..)
  , ConnParams(..)
  , withConnection

    -- ** Reconnection policy
  , ReconnectPolicy(..)
  , exponentialBackoff

    -- ** Connection parameters
  , Scheme(..)
  , Address(..)

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

    -- ** Low-level\/specialized API
  , isCallHealthy
  , sendInputWithEnvelope
  , recvOutputWithEnvelope

    -- * Communication patterns
  , rpc
  , rpcWith

    -- * Exceptions
  , ServerDisconnected(..)

    -- * Debugging
  , ClientDebugMsg(..)
  ) where

import Network.GRPC.Client.Call
import Network.GRPC.Client.Connection
import Network.GRPC.Client.StreamType (rpc, rpcWith)
import Network.GRPC.Spec
import Network.GRPC.Util.HTTP2.Stream (ServerDisconnected(..))
import Network.GRPC.Util.TLS qualified as Util.TLS

{-------------------------------------------------------------------------------
  Ongoing calls
-------------------------------------------------------------------------------}

-- $openRequest
--
-- 'Call' denotes a previously opened request (see 'withRPC').
--
-- == Protobuf communication patterns
--
-- This is a general implementation of the
-- [gRPC specification](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md).
-- As such, these functions do not provide explicit support for the common
-- communication patterns of non-streaming, server-side streaming, client-side
-- streaming, or bidirectional streaming. These are not part of the gRPC
-- standard, but are part of
-- [its Protobuf instantiation](https://protobuf.dev/reference/protobuf/proto3-spec/#service_definition), although these
-- patterns are of course not really Protobuf specific. We provide support for
-- these communication patterns, independent from a choice of serialization
-- format, in "Network.GRPC.Common.StreamType" and
-- "Network.GRPC.Client.StreamType" (and "Network.GRPC.Server.StreamType" for the
-- server side).
--
-- If you only use the abstractions provided in "Network.GRPC.*.StreamType",
-- you can ignore the rest of the discussion below, which applies only to the
-- more general interface.
--
-- == Stream elements
--
-- Both 'sendInput' and 'recvOutput' work with 'StreamElem':
--
-- > data StreamElem b a =
-- >     StreamElem a
-- >   | FinalElem a b
-- >   | NoMoreElems b
--
-- The intuition is that we are sending messages of type @a@ (see \"Inputs and
-- outputs\", below) and then when we send the final message, we can include
-- some additional information of type @b@ (see \"Metadata\", below).
--
-- == Inputs and outputs
--
-- By convention, we refer to messages sent from the client to the server as
-- \"inputs\" and messages sent from the server to the client as \"outputs\"
-- (we inherited this terminology from
-- [proto-lens](https://hackage.haskell.org/package/proto-lens-0.7.1.4/docs/Data-ProtoLens-Service-Types.html#t:HasMethodImpl).)
-- On the client side we therefore have 'recvOutput' and 'sendInput' defined as
--
-- > recvOutput :: Call rpc -> IO (StreamElem [CustomMetadata] (Output rpc))
-- > sendInput  :: Call rpc -> StreamElem NoMetadata (Input rpc) -> IO ()
--
-- and on the server side we have 'Network.GRPC.Server.recvInput' and
-- 'Network.GRPC.Server.sendOutput':
--
-- > recvInput  :: Call rpc -> IO (StreamElem NoMetadata (Input rpc))
-- > sendOutput :: Call rpc -> StreamElem [CustomMetadata] (Output rpc) -> IO ()
--
-- == Metadata
--
-- Both the server and the client can send some metadata before they send their
-- first message; see 'withRPC' and 'callRequestMetadata' for the client-side
-- (and 'Network.GRPC.Server.setResponseMetadata' for the server-side).
--
-- The gRPC specification allows the server, but not the client, to include some
-- /final/ metadata as well; this is the reason between the use of
-- 'CustomMetadata' for messages from the server to the client versus
-- 'NoMetadata' for messages from the client.
--
-- == 'FinalElem' versus 'NoMoreElems'
--
-- 'Network.GRPC.Common.StreamElem' allows to mark the final message as final
-- when it is /sent/ ('Network.GRPC.Common.FinalElem'), or retroactively
-- indicate that the previous message was in fact final
-- ('Network.GRPC.Common.NoMoreElems'). The reason for this is technical in
-- nature.
--
-- Suppose we are doing a @grpc+proto@ non-streaming RPC call. The input message
-- from the client to the server will be sent over one or more HTTP2 @DATA@
-- frames (chunks of the input). The server will expect the last of those frames
-- to be marked as @END_STREAM@. The HTTP2 specification /does/ allow sending an
-- separate empty @DATA@ frame with the @END_STREAM@ flag set to indicate no
-- further data is coming, but not all gRPC servers will wait for this, and
-- might either think that the client is broken and disconnect, or might send
-- the client a @RST_STREAM@ frame to force it to close the stream. To avoid
-- problems, therefore, it is better to mark the final @DATA@ frame as
-- @END_STREAM@; in order to be able to do that, 'sendInput' needs to know
-- whether an input is the final one. It is therefore better to use 'FinalElem'
-- instead of 'NoMoreElems' for outgoing messages, if possible.
--
-- For incoming messages the situation is different. Now we /do/ expect HTTP
-- trailers (final metadata), which means that we cannot tell from @DATA@ frames
-- alone if we have received the last message: it will be the frame containing
-- the /trailers/ that is marked as @END_STREAM@, with no indication on the data
-- frame just before it that it was the last one. We cannot wait for the next
-- frame to come in, because that would be a blocking call (we might have to
-- wait for the next TCP packet), and if the output was /not/ the last one, we
-- would unnecessarily delay making the output we already received available to
-- the client code. Typically therefore clients will receive a 'StreamElem'
-- followed by 'NoMoreElems'.
--
-- Of course, for a given RPC and its associated communication pattern we may
-- know whether it any given message was the last; in the example above of a
-- non-streaming @grpc+proto@ RPC call, we only expect a single output. In this
-- case the client can (and should) call 'recvOutput' again to wait for the
-- trailers (which, amongst other things, will include the 'trailerGrpcStatus').
-- The specialized functions from "Network.GRPC.Client.StreamType" take care of
-- this; if these functions are not applicable, users may wish to use
-- 'recvFinalOutput'.
