-- | Serialization functions
--
-- We collect these functions in a separate module, rather than exporting them
-- from "Network.GRPC.Spec", because while the functions in "Network.GRPC.Spec"
-- /may/ be needed in some user code (albeit rarely), the serialization
-- functions from this module really should only be needed in gRPC
-- implementations such as @grapesy@.
module Network.GRPC.Spec.Serialization (
    -- * Messages
    -- ** Inputs
    buildInput
  , parseInput
    -- ** Outputs
  , buildOutput
  , parseOutput
    -- * Headers
    -- ** Pseudoheaders
  , RawResourceHeaders(..)
  , InvalidResourceHeaders(..)
  , buildResourceHeaders
  , parseResourceHeaders
    -- ** RequestHeaders
  , buildRequestHeaders
  , parseRequestHeaders
  , parseRequestHeaders'
    -- *** Timeouts
  , buildTimeout
  , parseTimeout
    -- *** OpenTelemetry
  , buildTraceContext
  , parseTraceContext
    -- ** ResponseHeaders
  , buildResponseHeaders
  , parseResponseHeaders
  , parseResponseHeaders'
    -- *** Pushback
  , buildPushback
  , parsePushback
    -- ** ProperTrailers
  , buildProperTrailers
  , parseProperTrailers
  , parseProperTrailers'
    -- ** TrailersOnly
  , buildTrailersOnly
  , parseTrailersOnly
  , parseTrailersOnly'
    -- ** Classify server response
  , classifyServerResponse
    -- ** Custom metadata
  , parseCustomMetadata
  , buildCustomMetadata
    -- *** Binary values
  , buildBinaryValue
  , parseBinaryValue
    -- *** Status (Protobuf specific)
  , buildStatus
  , parseStatus
  ) where

import Network.GRPC.Spec.Serialization.CustomMetadata
import Network.GRPC.Spec.Serialization.Headers.PseudoHeaders
import Network.GRPC.Spec.Serialization.Headers.Request
import Network.GRPC.Spec.Serialization.Headers.Response
import Network.GRPC.Spec.Serialization.LengthPrefixed
import Network.GRPC.Spec.Serialization.Status
import Network.GRPC.Spec.Serialization.Timeout
import Network.GRPC.Spec.Serialization.TraceContext
