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
import Network.GRPC.Compression (CompressionId, Compression)
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

      -- | Initial set of accepted compression algorithms
      --
      -- This is used for inbound messages before the server has told us which
      -- compression algorithms it supports.
      --
      -- For outbound messages we will not use compression until the server has
      -- reported which compression algorithms it supported; when it does,
      -- 'connUpdateCompression' is called.
    , connInitCompression :: NonEmpty Compression

      -- | Update compression preferences
      --
      -- When this function returns 'Nothing', the connection to the server will
      -- be closed with an 'UnsupportedCompression' exception.
      --
      -- This function will only be called once, when the server gives their
      -- initial list of supported compression algorithms. The chosen algorithm
      -- is then used for the duration of the connection.
    , connUpdateCompression ::
          [CompressionId]
          -- ^ Compression algorithms the server reports as supported
        -> Either Compression.UnsupportedCompression Compression.Preference

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
    , connInitCompression   = Compression.allSupported
    , connUpdateCompression = Compression.defaultPreference
    , connScheme            = Http
    , connAuthority
    }
