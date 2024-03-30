module Network.GRPC.Spec.CustomMetadata.Typed (
    -- * Map RPC to metadata
    RequestMetadata
  , ResponseInitialMetadata
  , ResponseTrailingMetadata
  , ResponseMetadata(..)
    -- * Serialization
  , BuildMetadata(..)
  , StaticMetadata(..)
  , ParseMetadata(..)
    -- * Escape hatch: raw metadata
  , RawMetadata(..)
  ) where

import Control.Monad.Catch
import Data.Kind
import Data.Proxy
import Data.String
import GHC.TypeLits

import Network.GRPC.Spec.CustomMetadata.Raw

{-------------------------------------------------------------------------------
  Map RPC to metadata
-------------------------------------------------------------------------------}

-- | Metadata included in the request
--
-- Often you can give a blanket metadata definition for all methods in a
-- service. For example:
--
-- > type instance RequestMetadata          (Protobuf RouteGuide meth) = NoMetadata
-- > type instance ResponseInitialMetadata  (Protobuf RouteGuide meth) = NoMetadata
-- > type instance ResponseTrailingMetadata (Protobuf RouteGuide meth) = NoMetadata
--
-- If you want to give specific types of metadata for specific methods but not
-- for others, it can sometimes be useful to introduce an auxiliary closed type,
-- so that you can give a catch-all case. For example:
--
-- > type instance ResponseInitialMetadata (Protobuf Greeter meth) = GreeterResponseInitialMetadata meth
-- >
-- > type family GreeterResponseInitialMetadata (meth :: Symbol) where
-- >   GreeterResponseInitialMetadata "sayHelloStreamReply" = SayHelloMetadata
-- >   GreeterResponseInitialMetadata meth                  = NoMetadata
type family RequestMetadata (rpc :: k) :: Type

-- | Metadata included in the initial response
--
-- See 'RequestMetadata' for discussion.
type family ResponseInitialMetadata (rpc :: k) :: Type

-- | Metadata included in the response trailers
--
-- See 'RequestMetadata' for discussion.
type family ResponseTrailingMetadata (rpc :: k) :: Type

-- | Response metadata
--
-- It occassionally happens that we do not know if we should expect the initial
-- metadata from the server or the trailing metadata (when the server uses
-- Trailers-Only); for example, see
-- 'Network.GRPC.Client.recvResponseInitialMetadata'.
data ResponseMetadata rpc =
    ResponseInitialMetadata  (ResponseInitialMetadata  rpc)
  | ResponseTrailingMetadata (ResponseTrailingMetadata rpc)

deriving stock instance
     ( Show (ResponseInitialMetadata rpc)
     , Show (ResponseTrailingMetadata rpc)
     )
  => Show (ResponseMetadata rpc)

deriving stock instance
     ( Eq (ResponseInitialMetadata rpc)
     , Eq (ResponseTrailingMetadata rpc)
     )
  => Eq (ResponseMetadata rpc)

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

-- | Serialize metadata to custom metadata headers
class BuildMetadata a where
  buildMetadata :: a -> [CustomMetadata]

-- | Metadata with statically known fields
--
-- This is required for the response trailing metadata. When the server sends
-- the /initial/ set of headers to the client, it must tell the client which
-- trailers to expect (by means of the HTTP @Trailer@ header; see
-- <https://datatracker.ietf.org/doc/html/rfc7230#section-4.4>).
--
-- Any headers constructed in 'buildMetadata' /must/ be listed here; not doing
-- so is a bug. However, the converse is not true: it is acceptable for a header
-- to be listed in 'metadataHeaderNames' but not in 'buildMetadata'. Put another
-- way: the list of "trailers to expect" included in the initial request headers
-- is allowed to be an overapproximation, but not an underapproximation.
class BuildMetadata a => StaticMetadata a where
  metadataHeaderNames :: Proxy a -> [HeaderName]

-- | Parse metadata from custom metadata headers
--
-- This method is allowed to assume the list of headers does not contain any
-- duplicates (the gRPC spec does allow for duplicate headers and specifies
-- how to process them, but @grapesy@ will take care of this before calling
-- 'parseMetadata').
class ParseMetadata a where
  parseMetadata :: MonadThrow m => [CustomMetadata] -> m a

{-------------------------------------------------------------------------------
  Clients that want access to all raw, unparsed, custom metadata
-------------------------------------------------------------------------------}

-- | Raw metadata
--
-- This can be used with 'OverrideMetadata' to provide essentially an untyped
-- interface to custom metadata, and get access to the raw metadata without
-- any serialization.
newtype RawMetadata (md :: [Symbol]) = RawMetadata {
      getRawMetadata :: [CustomMetadata]
    }
  deriving stock (Show, Eq)

instance BuildMetadata (RawMetadata md) where
  buildMetadata = getRawMetadata

instance KnownSymbols md => StaticMetadata (RawMetadata md) where
  metadataHeaderNames _ = map fromString (symbolVals (Proxy @md))

instance ParseMetadata (RawMetadata md) where
  parseMetadata = return . RawMetadata

{-------------------------------------------------------------------------------
  Auxiliary: extend 'KnownSymbol' to a list of symbols
-------------------------------------------------------------------------------}

class KnownSymbols (ns :: [Symbol]) where
  symbolVals :: Proxy ns -> [String]

instance KnownSymbols '[] where
  symbolVals _ = []

instance (KnownSymbol n, KnownSymbols ns) => KnownSymbols (n:ns) where
  symbolVals _ = symbolVal (Proxy @n) : symbolVals (Proxy @ns)
