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
    -- * Specific instances
  , RawMetadata(..)
    -- ** Overriding metadata
  , OverrideMetadata
  , OverrideRequestMetadata
  , OverrideResponseInitialMetadata
  , OverrideResponseTrailingMetadata
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
type family RequestMetadata (rpc :: k) :: Type

-- | Metadata included in the initial response
type family ResponseInitialMetadata (rpc :: k) :: Type

-- | Metadata included in the response trailers
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
  Override metadata
-------------------------------------------------------------------------------}

-- | Override metadata associated with a given RPC
--
-- The standard RPC instances (most importantly,
-- 'Network.GRPC.Common.Protobuf.Protobuf') do not have any standard associated
-- metadata. You can use 'OverrideMetadata' to change this.
--
-- As an example, consider these definitions from the @grapesy@ interop test
-- suite (<https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md>):
--
-- > type WithInteropMeta =
-- >        OverrideMetadata
-- >          InteropReqMeta
-- >          InteropRespInitMeta
-- >          InteropRespTrailMeta
-- >
-- > type UnaryCall = WithInteropMeta (Protobuf TestService "unaryCall")
--
-- By default, @(Protobuf TestService "unaryCall")@ has no associated metadata;
-- the 'WithInteropMeta' type synonym here is used to use @InteropReqMeta@
-- for the request metadata, @InteropRespInitMeta@ for the response initial
-- metadata, and @InteropRespTrailMeta@ for the response trailing metadata.
--
-- When you override metadata, you will need to provide some instances. For
-- clients, you will need to provide
--
-- * 'BuildMetadata' instance for @req@
-- * 'ParseMetadata' instances for @init@ and @trail@
--
-- You will probably also want to provide a 'Data.Default.Default' instance for
-- @req@, unless every RPC call must be given explicit request metadata
-- (see 'Network.GRPC.Client.rpc' versus 'Network.GRPC.Client.rpcWith').
--
-- On the server side, you will need to provide
--
-- * 'ParseMetadata' instance for @req@
-- * 'BuildMetadata' instances for @init@ and @trail@
-- * 'StaticMetadata' instance for @trail@
--
-- You will probably also want to provide a 'Data.Default.Default' instance for
-- @init@ and @trail@. If you do not, you cannot
-- 'Network.GRPC.Server.mkRpcHandler' or any of the high level handler
-- constructions, and you need to use
-- 'Network.GRPC.Server.mkRpcHandlerNoInitialMetadata' instead.
--
-- See 'RawMetadata' for getting access to the raw custom metadata headers.
data OverrideMetadata (req :: Type) (init :: Type) (trail :: Type) rpc

type instance RequestMetadata          (OverrideMetadata req init trail rpc) = req
type instance ResponseInitialMetadata  (OverrideMetadata req init trail rpc) = init
type instance ResponseTrailingMetadata (OverrideMetadata req init trail rpc) = trail

-- | Alias for 'OverrideMeta' to override /only/ the request metadata
type OverrideRequestMetadata req rpc =
       OverrideMetadata
         req
         (ResponseInitialMetadata rpc)
         (ResponseTrailingMetadata rpc)
         rpc

-- | Alias for 'OverrideMeta' to override /only/ the response initial metadata
type OverrideResponseInitialMetadata init rpc =
       OverrideMetadata
         (RequestMetadata rpc)
         init
         (ResponseTrailingMetadata rpc)
         rpc

-- | Alias for 'OverrideMeta' to override /only/ the response trailing metadata
type OverrideResponseTrailingMetadata trail rpc =
       OverrideMetadata
         (RequestMetadata rpc)
         (ResponseInitialMetadata rpc)
         trail
         rpc

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
