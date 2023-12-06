{-# LANGUAGE OverloadedStrings #-}

-- | Functionality shared between requests and responses
--
-- The following headers are used both in requests and in responses:
--
-- * @Content-Type@
-- * @Message-Encoding@
-- * @Message-Accept-Encoding@
-- * @Custom-Metadata@ (see "Network.GRPC.Spec.CustomMetadata")
--
-- Intended for unqualified import.
module Network.GRPC.Spec.Common (
    -- * Construction
    buildContentType
  , buildMessageEncoding
  , buildMessageAcceptEncoding
    -- * Parsing
  , parseContentType
  ) where

import Control.Monad.Except
import Data.Proxy
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Partial

{-------------------------------------------------------------------------------
  > Content-Type →
  >   "content-type"
  >   "application/grpc"
  >   [("+proto" / "+json" / {custom})]
-------------------------------------------------------------------------------}

buildContentType :: IsRPC rpc => Proxy rpc -> HTTP.Header
buildContentType proxy = (
      "content-type"
    , "application/grpc+" <> serializationFormat proxy
    )

parseContentType ::
     (MonadError String m, IsRPC rpc)
  => Proxy rpc
  -> HTTP.Header
  -> m ()
parseContentType proxy hdr =
    expectHeaderValue hdr $ [
        "application/grpc"
      , "application/grpc+" <> serializationFormat proxy
      ]

{-------------------------------------------------------------------------------
  > Message-Encoding → "grpc-encoding" Content-Coding
  > Content-Coding → "identity" / "gzip" / "deflate" / "snappy" / {custom}
-------------------------------------------------------------------------------}

buildMessageEncoding :: HTTP.Header
buildMessageEncoding = (
      "grpc-encoding"
    , "identity"
    )

{-------------------------------------------------------------------------------
  > Message-Accept-Encoding →
  >   "grpc-accept-encoding" Content-Coding *("," Content-Coding)
-------------------------------------------------------------------------------}

buildMessageAcceptEncoding :: HTTP.Header
buildMessageAcceptEncoding = (
      "grpc-accept-encoding"
    , "identity"
    )
