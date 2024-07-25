{-# LANGUAGE CPP #-}

-- | Pure implementation of the gRPC spec
--
-- Most code will not need to use this module directly.
--
-- Intended for unqualified import.
module Network.GRPC.Spec (
    -- * RPC
    IsRPC(..)
  , Input
  , Output
  , SupportsClientRpc(..)
  , SupportsServerRpc(..)
  , defaultRpcContentType
    -- ** Instances
    -- *** Protobuf
  , Protobuf
  , Proto(..)
  , getProto
    -- *** JSON
  , JsonRpc
  , JsonObject(..)
  , Required(..)
  , Optional(..)
    -- *** Raw
  , RawRpc
    -- *** Unknown
  , UnknownRpc
    -- * Streaming types
  , StreamingType(..)
  , SStreamingType(..)
  , ValidStreamingType(..)
    -- ** Link RPCs to streaming types
  , SupportsStreamingType
  , HasStreamingType(..)
    -- ** Handler type definition
    -- These are not used directly in grapesy's public API.
  , Send
  , Recv
  , Positive
  , Negative(..)
  , HandlerRole(..)
  , Handler
    -- ** Handler newtype wrappers
  , ServerHandler'(..)
  , ServerHandler
  , ClientHandler'(..)
  , ClientHandler
  , hoistServerHandler
    -- * Compression
  , CompressionId(..)
  , Compression(..)
  , serializeCompressionId
  , deserializeCompressionId
    -- ** Compression algorithms
  , noCompression
  , gzip
  , deflate
#ifdef SNAPPY
  , snappy
#endif
  , allSupportedCompression
    -- * Message metadata
  , OutboundMeta(..)
  , InboundMeta(..)
    -- * Requests
  , RequestHeaders_(..)
  , RequestHeaders
  , RequestHeaders'
    -- ** Parameters
  , CallParams(..)
    -- ** Pseudo-headers
  , PseudoHeaders(..)
  , ServerHeaders(..)
  , ResourceHeaders(..)
  , Path(..)
  , Address(..)
  , Scheme(..)
  , Method(..)
  , rpcPath
    -- ** Timeouts
  , Timeout(..)
  , TimeoutValue(..)
  , TimeoutUnit(..)
  , timeoutToMicro
  , isValidTimeoutValue
    -- * Responses
    -- ** Headers
  , ResponseHeaders_(..)
  , ResponseHeaders
  , ResponseHeaders'
    -- ** Trailers
  , ProperTrailers_(..)
  , ProperTrailers
  , ProperTrailers'
  , TrailersOnly_(..)
  , TrailersOnly
  , TrailersOnly'
  , Pushback(..)
  , simpleProperTrailers
    -- ** Termination
  , GrpcNormalTermination(..)
  , grpcExceptionToTrailers
  , grpcClassifyTermination
  , properTrailersToTrailersOnly
  , trailersOnlyToProperTrailers
    -- * Status
  , GrpcStatus(..)
  , GrpcError(..)
    -- ** Exceptions
  , GrpcException(..)
  , throwGrpcError
    -- * Metadata
  , CustomMetadata(CustomMetadata)
  , customMetadataName
  , customMetadataValue
  , safeCustomMetadata
  , HeaderName(BinaryHeader, AsciiHeader)
  , safeHeaderName
  , isValidAsciiValue
  , NoMetadata(..)
  , UnexpectedMetadata(..)
    -- ** Handling of duplicate metadata entries
  , CustomMetadataMap -- opaque
  , customMetadataMapFromList
  , customMetadataMapToList
  , customMetadataMapInsert
    -- ** Typed
  , RequestMetadata
  , ResponseInitialMetadata
  , ResponseTrailingMetadata
  , ResponseMetadata(..)
  , RawMetadata(..)
    -- ** Serialization
  , BuildMetadata(..)
  , ParseMetadata(..)
  , StaticMetadata(..)
  , buildMetadataIO
    -- * Invalid headers
  , InvalidHeaders(..)
  , InvalidHeader(..)
    -- ** Construction
  , invalidHeader
  , invalidHeaderWith
  , missingHeader
  , unexpectedHeader
  , throwInvalidHeader
    -- ** Use
  , prettyInvalidHeaders
  , statusInvalidHeaders
    -- * Common infrastructure to all headers
  , ContentType(..)
  , MessageType(..)
    -- * OpenTelemetry
  , TraceContext(..)
  , TraceId(..)
  , SpanId(..)
  , TraceOptions(..)
    -- * ORCA
  , OrcaLoadReport
  ) where

import Network.GRPC.Spec.Call
import Network.GRPC.Spec.Compression
import Network.GRPC.Spec.CustomMetadata.Map
import Network.GRPC.Spec.CustomMetadata.NoMetadata
import Network.GRPC.Spec.CustomMetadata.Raw
import Network.GRPC.Spec.CustomMetadata.Typed
import Network.GRPC.Spec.Headers.Common
import Network.GRPC.Spec.Headers.Invalid
import Network.GRPC.Spec.Headers.PseudoHeaders
import Network.GRPC.Spec.Headers.Request
import Network.GRPC.Spec.Headers.Response
import Network.GRPC.Spec.MessageMeta
import Network.GRPC.Spec.OrcaLoadReport
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.JSON
import Network.GRPC.Spec.RPC.Protobuf
import Network.GRPC.Spec.RPC.Raw
import Network.GRPC.Spec.RPC.StreamType
import Network.GRPC.Spec.RPC.Unknown
import Network.GRPC.Spec.Status
import Network.GRPC.Spec.Timeout
import Network.GRPC.Spec.TraceContext
