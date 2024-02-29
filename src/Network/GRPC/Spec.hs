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
  , RequestHeaders(..)
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
  , ResponseHeaders(..)
  , buildResponseHeaders
  , parseResponseHeaders
    -- ** Trailers
  , ProperTrailers(..)
  , TrailersOnly(..)
    -- ** Serialization
  , parseProperTrailers
  , parseTrailersOnly
  , buildProperTrailers
  , buildTrailersOnly
  , properTrailersToTrailersOnly
  , trailersOnlyToProperTrailers
    -- *** Status
  , GrpcStatus(..)
  , GrpcError(..)
  , fromGrpcStatus
  , toGrpcStatus
    -- *** gRPC exceptions
  , GrpcException(..)
  , grpcExceptionToTrailers
  , grpcExceptionFromTrailers
    -- * Metadata
  , CustomMetadata
  , HeaderValue(..)
  , HeaderName(..)
  , BinaryValue(..)
  , AsciiValue(..)
  , NoMetadata(..)
    -- ** Serialization
  , buildBinaryValue
  , parseBinaryValue
  , parseCustomMetadata
  , buildCustomMetadata
    -- ** Validation
  , safeHeaderName
  , safeAsciiValue
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
import Network.GRPC.Spec.CustomMetadata
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
