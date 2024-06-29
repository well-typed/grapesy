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
    -- ** Messages
    -- *** Parsing
  , InboundEnvelope(..)
  , parseInput
  , parseOutput
    -- *** Construction
  , OutboundEnvelope(..)
  , buildInput
  , buildOutput
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
    -- * Requests
  , RequestHeaders_(..)
  , RequestHeaders
  , RequestHeaders'
  , InvalidRequestHeaders
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
    -- ** Serialization
  , RawResourceHeaders(..)
  , InvalidResourceHeaders(..)
  , buildResourceHeaders
  , parseResourceHeaders
    -- ** Headers
  , buildRequestHeaders
  , parseRequestHeaders
  , parseRequestHeaders'
    -- ** Timeouts
  , Timeout(..)
  , TimeoutValue(..)
  , TimeoutUnit(..)
  , timeoutToMicro
  , isValidTimeoutValue
    -- ** Serialization
  , buildTimeout
  , parseTimeout
    -- * Responses
    -- ** Headers
  , ResponseHeaders_(..)
  , ResponseHeaders
  , ResponseHeaders'
  , buildResponseHeaders
  , parseResponseHeaders
  , parseResponseHeaders'
    -- ** Trailers
  , ProperTrailers_(..)
  , ProperTrailers
  , ProperTrailers'
  , TrailersOnly_(..)
  , TrailersOnly
  , TrailersOnly'
  , Pushback(..)
  , simpleProperTrailers
    -- ** Serialization
  , parseProperTrailers
  , parseProperTrailers'
  , parseTrailersOnly
  , parseTrailersOnly'
  , parsePushback
  , buildProperTrailers
  , buildTrailersOnly
  , buildPushback
  , properTrailersToTrailersOnly
  , trailersOnlyToProperTrailers
    -- *** Status
  , GrpcStatus(..)
  , GrpcError(..)
  , fromGrpcStatus
  , toGrpcStatus
    -- *** gRPC termination
  , GrpcException(..)
  , throwGrpcError
  , GrpcNormalTermination(..)
  , grpcExceptionToTrailers
  , grpcClassifyTermination
    -- * Metadata
  , CustomMetadata(CustomMetadata)
  , customMetadataName
  , customMetadataValue
  , safeCustomMetadata
  , HeaderName(BinaryHeader, AsciiHeader)
  , safeHeaderName
  , NoMetadata(..)
  , UnexpectedMetadata(..)
    -- ** Handling of duplicate metadata entries
  , CustomMetadataMap -- opaque
  , customMetadataMapFromList
  , customMetadataMapToList
  , customMetadataMapInsert
    -- ** Serialization
  , buildBinaryValue
  , parseBinaryValue
  , parseCustomMetadata
  , buildCustomMetadata
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
    -- * Common infrastructure to all headers
  , InvalidHeaders(..)
  , InvalidHeader(..)
  , prettyInvalidHeaders
  , ContentType(..)
  , MessageType(..)
    -- * OpenTelemetry
  , TraceContext(..)
  , TraceId(..)
  , SpanId(..)
  , TraceOptions(..)
  , buildTraceContext
  , parseTraceContext
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
import Network.GRPC.Spec.LengthPrefixed
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
