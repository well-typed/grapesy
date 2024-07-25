-- | Construct HTTP2 requests
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.Request qualified as Req
module Network.GRPC.Spec.Headers.Request (
    -- * Inputs (message sent to the peer)
    RequestHeaders_(..)
  , RequestHeaders
  , RequestHeaders'
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

import Network.GRPC.Spec.Compression (CompressionId)
import Network.GRPC.Spec.CustomMetadata.Map
import Network.GRPC.Spec.Headers.Common
import Network.GRPC.Spec.Headers.Invalid
import Network.GRPC.Spec.Timeout
import Network.GRPC.Spec.TraceContext
import Network.GRPC.Util.HKD (HKD, Undecorated, Checked)
import Network.GRPC.Util.HKD qualified as HKD

{-------------------------------------------------------------------------------
  Inputs (message sent to the peer)
-------------------------------------------------------------------------------}

-- | Full set of call parameters required to construct the RPC call
--
-- This is constructed internally; it is not part of the public API.
data RequestHeaders_ f = RequestHeaders {
      -- | Timeout
      requestTimeout :: HKD f (Maybe Timeout)

      -- | Compression used for outgoing messages
    , requestCompression :: HKD f (Maybe CompressionId)

      -- | Accepted compression algorithms for incoming messages
      --
      -- @Maybe (NonEmpty ..)@ is perhaps a bit strange (why not just @[]@), but
      -- it emphasizes the specification: /if/ the header is present, it must be
      -- a non-empty list.
    , requestAcceptCompression :: HKD f (Maybe (NonEmpty CompressionId))

      -- | Optionally, override the content-type
      --
      -- Set to 'Nothing' to omit the content-type header altogether.
      --
      -- See also discussion of 'requestMessageType'.
    , requestContentType :: HKD f (Maybe ContentType)

      -- | Should we include the @Message-Type@ header?
      --
      -- Set to 'Nothing' to omit the message-type header altogether.
      --
      -- We do not need the header in order to know the message type, because
      -- the /path/ determines the service and method, and that in turn
      -- determines the message type. If it /is/ present, however, we verify
      -- that it has the valeu we expect.
    , requestMessageType :: HKD f (Maybe MessageType)

      -- | User agent
    , requestUserAgent :: HKD f (Maybe Strict.ByteString)

      -- | Should we include the @te: trailers@ header?
      --
      -- The @TE@ header is part of the HTTP specification;
      -- see also <https://datatracker.ietf.org/doc/html/rfc7230#section-4.3>.
      -- It indicates that we are willing to accept a chunked encoding for the
      -- response body, and that we expect trailers to be present after the
      -- response body.
      --
      -- To be conform to the gRPC spec, the @te@ header should be included, but
      -- @grapesy@ does not insist that the header is present for incoming
      -- requests. However, /if/ it is present, we /do/ verify that it has the
      -- right value; this prevents values getting lost without notice.
    , requestIncludeTE :: HKD f Bool

      -- | Trace context (for OpenTelemetry)
    , requestTraceContext :: HKD f (Maybe TraceContext)

      -- | Previous RPC attempts
      --
      -- This is part of automatic retries.
      -- See <https://github.com/grpc/proposal/blob/master/A6-client-retries.md>.
    , requestPreviousRpcAttempts :: HKD f (Maybe Int)

      -- | Custom metadata
      --
      -- Any header we do not otherwise explicitly support we attempt to parse
      -- as custom metadata. Headers for which this fails end up in
      -- 'requestUnrecognized'; reasons for this include the use of reserved
      -- header names (starting with @grpc-@), invalid binary encodings, etc.
    , requestMetadata :: CustomMetadataMap

      -- | Unrecognized headers
    , requestUnrecognized :: HKD f ()
    }
  deriving anyclass (HKD.Coerce)

-- | Request headers (without allowing for invalid headers)
--
-- NOTE: The HKD type
--
-- > RequestHeaders_ Undecorated
--
-- means that each field of type @HKD f a@ is simply of type @a@ (that is,
-- undecorated).
type RequestHeaders = RequestHeaders_ Undecorated

-- | Request headers allowing for invalid headers
--
-- NOTE: The HKD type
--
-- > RequestHeaders_ (Checked InvalidHeaders)
--
-- means that each field of type @HKD f a@ is of type
--
-- > Either InvalidHeaders a
--
-- (i.e., either valid or invalid).
type RequestHeaders' = RequestHeaders_ (Checked InvalidHeaders)

deriving stock instance Show    RequestHeaders
deriving stock instance Eq      RequestHeaders
deriving stock instance Generic RequestHeaders

deriving stock instance Show RequestHeaders'
deriving stock instance Eq   RequestHeaders'
-- We do not derive Generic for RequestHeaders', as doing so makes ghc confused
-- about the instance for RequestHeaders for some reason.

instance HKD.Traversable RequestHeaders_ where
  traverse f x =
      RequestHeaders
        <$> (f    $ requestTimeout             x)
        <*> (f    $ requestCompression         x)
        <*> (f    $ requestAcceptCompression   x)
        <*> (f    $ requestContentType         x)
        <*> (f    $ requestMessageType         x)
        <*> (f    $ requestUserAgent           x)
        <*> (f    $ requestIncludeTE           x)
        <*> (f    $ requestTraceContext        x)
        <*> (f    $ requestPreviousRpcAttempts x)
        <*> (pure $ requestMetadata            x)
        <*> (f    $ requestUnrecognized        x)
