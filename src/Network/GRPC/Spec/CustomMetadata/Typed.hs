module Network.GRPC.Spec.CustomMetadata.Typed (
    -- * API
    BuildMetadata(..)
  , StaticMetadata(..)
  , ParseMetadata(..)
    -- * Specific instances
  , RawMetadata(..)
    -- * Map RPC to metadata
  , HasCustomMetadata(..)
  , ResponseMetadata(..)
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
  API

  TODO: Docs
-------------------------------------------------------------------------------}

class BuildMetadata a where
  buildMetadata :: a -> [CustomMetadata]

class BuildMetadata a => StaticMetadata a where
  metadataHeaderNames :: Proxy a -> [HeaderName]

-- TODO: Docs should say something about duplicate headers
class ParseMetadata a where
  parseMetadata :: MonadThrow m => [CustomMetadata] -> m a

{-------------------------------------------------------------------------------
  Clients that want access to all raw, unparsed, custom metadata
-------------------------------------------------------------------------------}

-- TODO: Docs
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

{-------------------------------------------------------------------------------
  Map RPC to metadata
-------------------------------------------------------------------------------}

class ( -- 'Show' instances (for debugging)
        Show (RequestMetadata rpc)
      , Show (ResponseInitialMetadata rpc)
      , Show (ResponseTrailingMetadata rpc)
      ) => HasCustomMetadata rpc where
  -- | Metadata included in the request
  type RequestMetadata rpc :: Type

  -- | Metadata included in the initial response
  type ResponseInitialMetadata rpc :: Type

  -- | Metadata included in the response trailers
  type ResponseTrailingMetadata rpc :: Type

-- TODO: Docs
data ResponseMetadata rpc =
    ResponseInitialMetadata  (ResponseInitialMetadata  rpc)
  | ResponseTrailingMetadata (ResponseTrailingMetadata rpc)

deriving stock instance
     HasCustomMetadata rpc
  => Show (ResponseMetadata rpc)

deriving stock instance
     ( Eq (ResponseInitialMetadata rpc)
     , Eq (ResponseTrailingMetadata rpc)
     )
  => Eq (ResponseMetadata rpc)

{-------------------------------------------------------------------------------
  Override metadata

  TODO: Docs
-------------------------------------------------------------------------------}

data OverrideMetadata (req :: Type) (init :: Type) (trail :: Type) rpc

instance ( Show req
         , Show init
         , Show trail
         ) => HasCustomMetadata (OverrideMetadata req init trail rpc) where
  type RequestMetadata          (OverrideMetadata req init trail rpc) = req
  type ResponseInitialMetadata  (OverrideMetadata req init trail rpc) = init
  type ResponseTrailingMetadata (OverrideMetadata req init trail rpc) = trail

type OverrideRequestMetadata          req   rpc = OverrideMetadata req                   (ResponseInitialMetadata rpc) (ResponseTrailingMetadata rpc) rpc
type OverrideResponseInitialMetadata  init  rpc = OverrideMetadata (RequestMetadata rpc) init                          (ResponseTrailingMetadata rpc) rpc
type OverrideResponseTrailingMetadata trail rpc = OverrideMetadata (RequestMetadata rpc) (ResponseInitialMetadata rpc) trail                          rpc
