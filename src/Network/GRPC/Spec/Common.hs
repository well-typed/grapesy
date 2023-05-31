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
  , parseMessageEncoding
  , parseMessageAcceptEncoding
  ) where

import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Typeable
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec.Compression (CompressionId)
import Network.GRPC.Spec.Compression qualified as Compression
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.ByteString

{-------------------------------------------------------------------------------
  Preliminaries
-------------------------------------------------------------------------------}

type HeaderValue = Strict.ByteString

checkAtLeastOne :: forall a. Typeable a => [a] -> Either String (NonEmpty a)
checkAtLeastOne (x : xs) = Right (x :| xs)
checkAtLeastOne []       = Left $ concat [
                               "Expected at least one "
                             , show (typeRep (Proxy @a))
                             ]

{-------------------------------------------------------------------------------
  > Content-Type →
  >   "content-type"
  >   "application/grpc"
  >   [("+proto" / "+json" / {custom})]
-------------------------------------------------------------------------------}

buildContentType :: IsRPC rpc => rpc -> HTTP.Header
buildContentType rpc = (
      "content-type"
    , "application/grpc+" <> serializationFormat rpc
    )

parseContentType ::
     IsRPC rpc
  => rpc
  -> HeaderValue
  -> Either String ()
parseContentType rpc value =
    if value `elem` accepted then
      -- TODO: We should return the serialization format
      Right ()
    else
      Left $ concat [
          "Unexpected content-type "
        , BS.Strict.C8.unpack value
        , "; expected one of "
        , mconcat . intersperse ", " . map BS.Strict.C8.unpack $ accepted
        , "'"
        ]
  where
    accepted :: [Strict.ByteString]
    accepted = [
        "application/grpc"
      , "application/grpc+" <> serializationFormat rpc
      ]

{-------------------------------------------------------------------------------
  > Message-Encoding → "grpc-encoding" Content-Coding
  > Content-Coding → "identity" / "gzip" / "deflate" / "snappy" / {custom}
-------------------------------------------------------------------------------}

buildMessageEncoding :: CompressionId -> HTTP.Header
buildMessageEncoding compr = (
      "grpc-encoding"
    , Compression.serializeId compr
    )

parseMessageEncoding ::
     HeaderValue
  -> Either String CompressionId
parseMessageEncoding = Right . Compression.deserializeId

{-------------------------------------------------------------------------------
  > Message-Accept-Encoding →
  >   "grpc-accept-encoding" Content-Coding *("," Content-Coding)
-------------------------------------------------------------------------------}

buildMessageAcceptEncoding :: NonEmpty CompressionId -> HTTP.Header
buildMessageAcceptEncoding compr = (
      "grpc-accept-encoding"
    , mconcat . intersperse "," . map Compression.serializeId $ toList compr
    )

parseMessageAcceptEncoding ::
     HeaderValue
  -> Either String (NonEmpty CompressionId)
parseMessageAcceptEncoding =
      checkAtLeastOne
    . map (Compression.deserializeId . strip)
    . BS.Strict.splitWith (== ascii ',')



