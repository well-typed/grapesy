-- | Connection parameters
--
-- This is part of the public API; relevant parts are re-exported in
-- "Network.GRPC.Client".
--
-- Intended for unqualified import.
module Network.GRPC.Client.Connection.Params (
    -- * Definition
    ConnParams(..)
  , defaultConnParams
  ) where

import Control.Tracer
import Data.List.NonEmpty (NonEmpty)

import Network.GRPC.Client.Logging
import Network.GRPC.Compression (Compression)
import Network.GRPC.Compression qualified as Compression
import Network.GRPC.Spec.HTTP2.Connection

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data ConnParams = ConnParams {
      -- | Logging
      --
      -- Most applications will probably just set this to 'nullTracer' to
      -- disable logging.
      connTracer :: Tracer IO LogMsg

      -- | Supported compression algorithms
      --
      -- The order of the list matters:
      --
      -- * We will use the first algorithm in the list that is supported by
      --   the server for compression of outgoing messages.
      -- * We will present the list in-order to the server as accepted
      --   algorithms, which the server may interpret as a list in order of
      --   preference.
      --
      -- Until we have received a list of supported compression algorithms from
      -- the server, we will not use /any/ compression for outgoing messages.
    , connCompression :: NonEmpty Compression

      -- | HTTP scheme
    , connScheme :: Scheme

      -- | Server to connect to
    , connAuthority :: Authority
    }

-- | Default connection parameters
--
-- TODO: We should use https by default instead of http.
defaultConnParams :: Authority -> ConnParams
defaultConnParams connAuthority = ConnParams{
      connTracer            = nullTracer
    , connCompression       = Compression.allSupported
    , connScheme            = Http
    , connAuthority
    }
