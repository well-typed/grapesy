-- | Implementation of the GRPC spec
--
-- See <https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md>.
-- The spec uses ABNF syntax <https://www.rfc-editor.org/rfc/rfc5234>. Summary:
--
-- * Ordered sequence: A B C
-- * Alternatives: A / B / C
-- * Repetition: *A for zero-or-more, and 1*A for one-or-more
-- * Optional: [A]
--
-- In addition (not conform ABNF), the gRPC spec also uses ?A to mean optional.
--
-- Intended for unqualified import.
module Network.GRPC.Spec (
    -- * Requests
    RequestMeta(..)
  , Scheme(..)
  , Authority(..)
    -- ** Timeouts
  , Timeout(..)
  , TimeoutValue(TimeoutValue, getTimeoutValue)
  , TimeoutUnit(..)
    -- * Responses
  , ResponseHeaders(..)
  , ResponseTrailers(..)
  , uninitResponseHeaders
  , uninitResponseTrailers
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.Default
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import Generics.SOP qualified as SOP
import GHC.Generics qualified as GHC

import Network.GRPC.Spec.Compression (Compression(..), CompressionId)
import Network.GRPC.Spec.Compression qualified as Compression
import Network.GRPC.Spec.CustomMetadata

{-------------------------------------------------------------------------------
  Requests
-------------------------------------------------------------------------------}

-- | Metadata for a gRPC request
--
-- We omit the request scheme (http/https) and the authority; they are supplied
-- once and for all when we connect to the server.
data RequestMeta = RequestMeta {
      -- | Timeout
      --
      -- If Timeout is omitted a server should assume an infinite timeout.
      -- Client implementations are free to send a default minimum timeout based
      -- on their deployment requirements.
      requestTimeout :: Maybe Timeout

      -- | Custom metadata
    , requestCustomMetadata :: [CustomMetadata]

      -- | Compression for inputs (sent from the client to the server)
      --
      -- This corresponds to the gRPC header @Message-Encoding@, but we avoid
      -- the term \"encoding\", as it is ambiguous.
    , requestCompression :: Compression

      -- | Accepted compression for outputs (from the server to the client)
      --
      -- This corresponds to the gRPC header @Message-Accept-Encoding@.
    , requestAcceptCompression :: NonEmpty Compression
    }
  deriving stock (Show)

-- | Default 'RequestMeta'
--
-- The default 'requestInputCompression' is 'Compression.identity' (i.e.,
-- no compression). This is the only safe option until the server has told us
-- which compression schemes it supports.
instance Default RequestMeta where
  def = RequestMeta {
        requestTimeout           = Nothing
      , requestCustomMetadata    = []
      , requestCompression       = Compression.identity
      , requestAcceptCompression = Compression.allSupported
      }

data Scheme = Http | Https
  deriving stock (Show)

-- | HTTP authority
data Authority = Authority {
      authorityUserInfo :: Maybe String
    , authorityHost     :: String
    , authorityPort     :: Word
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Timeouts
-------------------------------------------------------------------------------}

data Timeout = Timeout TimeoutUnit TimeoutValue
  deriving stock (Show)

-- | Positive integer with ASCII representation of at most 8 digits
newtype TimeoutValue = UnsafeTimeoutValue {
      getTimeoutValue :: Word
    }
  deriving newtype (Show) -- relies on the Num instance

pattern TimeoutValue :: Word -> TimeoutValue
pattern TimeoutValue t <- UnsafeTimeoutValue t
  where
    TimeoutValue t
      | isValidTimeoutValue t = UnsafeTimeoutValue t
      | otherwise = error $ "invalid TimeoutValue: " ++ show t

{-# COMPLETE TimeoutValue #-}

isValidTimeoutValue :: Word -> Bool
isValidTimeoutValue t = length (show t) <= 8

data TimeoutUnit =
    Hour
  | Minute
  | Second
  | Millisecond
  | Microsecond
  | Nanosecond
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Responses
-------------------------------------------------------------------------------}

-- | Response headers
--
-- TODO: @grpc-encoding@ (for compression)
--
-- When we add this, should also update the parser.
data ResponseHeaders (f :: Type -> Type) = ResponseHeaders {
      responseContentType       :: f Strict.ByteString
    , responseAcceptCompression :: f (Maybe [CompressionId])
    , responseCustomMetadata    :: f [CustomMetadata]
    }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Response trailers
--
-- TODO: Custom metadata. When we add that, should also update the parser.
data ResponseTrailers (f :: Type -> Type) = ResponseTrailers {
      responseGrpcStatus  :: f Word
    , responseGrpcMessage :: f (Maybe String)
    }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

deriving instance (forall a. Show a => Show (f a)) => Show (ResponseHeaders  f)
deriving instance (forall a. Show a => Show (f a)) => Show (ResponseTrailers f)

uninitResponseHeaders :: ResponseHeaders (Either String)
uninitResponseHeaders = ResponseHeaders {
      responseContentType       = Left "Missing content-type"
    , responseAcceptCompression = Right Nothing
    , responseCustomMetadata    = Right []
    }

uninitResponseTrailers :: ResponseTrailers (Either String)
uninitResponseTrailers = ResponseTrailers {
      responseGrpcStatus  = Left "Missing grpc-status"
    , responseGrpcMessage = Right Nothing
    }