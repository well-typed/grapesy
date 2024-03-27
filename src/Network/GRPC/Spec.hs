{-# LANGUAGE CPP #-}

-- | Pure implementation of the gRPC spec
--
-- Most code will not need to use this module directly.
--
-- Intended for unqualified import.
module Network.GRPC.Spec (
    -- * RPC
    IsRPC(..)
  , defaultRpcContentType
    -- ** Instances
  , Protobuf
  , BinaryRpc
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
  , SupportsStreamingType
  , HasStreamingType(..)
    -- ** Handlers
  , HandlerFor
  , NonStreamingHandler(..)
  , ClientStreamingHandler(..)
  , ServerStreamingHandler(..)
  , BiDiStreamingHandler(..)
    -- ** Execution
  , nonStreaming
  , clientStreaming
  , serverStreaming
  , biDiStreaming
    -- ** Construction
  , mkNonStreaming
  , mkClientStreaming
  , mkServerStreaming
  , mkBiDiStreaming
    -- * Compression
  , CompressionId(..)
  , Compression(..)
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
  , buildResponseHeaders
  , parseResponseHeaders
    -- ** Trailers
  , ProperTrailers_(..)
  , ProperTrailers
  , TrailersOnly_(..)
  , TrailersOnly
  , Pushback(..)
    -- ** Serialization
  , parseProperTrailers
  , parseTrailersOnly
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
    -- * Content type
  , ContentType(..)
    -- * OpenTelemetry
  , TraceContext(..)
  , TraceId(..)
  , SpanId(..)
  , TraceOptions(..)
  , buildTraceContext
  , parseTraceContext
  ) where

import Network.GRPC.Spec.Call
import Network.GRPC.Spec.Common
import Network.GRPC.Spec.Compression
import Network.GRPC.Spec.CustomMetadata.Map
import Network.GRPC.Spec.CustomMetadata.Raw
import Network.GRPC.Spec.LengthPrefixed
import Network.GRPC.Spec.PseudoHeaders
import Network.GRPC.Spec.Request
import Network.GRPC.Spec.Response
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.Binary
import Network.GRPC.Spec.RPC.Protobuf
import Network.GRPC.Spec.RPC.StreamType
import Network.GRPC.Spec.RPC.Unknown
import Network.GRPC.Spec.Status
import Network.GRPC.Spec.Timeout
import Network.GRPC.Spec.TraceContext
