{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Spec.RPC.CBOR (CborRpc) where

import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise
import Control.DeepSeq (NFData)
import Control.Exception (displayException)
import Data.ByteString.Char8 qualified as BS.Char8
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Proxy
import GHC.TypeLits

import Network.GRPC.Spec.CustomMetadata.Typed
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.StreamType

{-------------------------------------------------------------------------------
  CBOR format
-------------------------------------------------------------------------------}

-- | gRPC using CBOR as the message encoding
--
-- You will need to manually provide 'Input' and 'Output' instances for each
-- RPC you use, along with 'Serialise' instances for those types.
data CborRpc (serv :: Symbol) (meth :: Symbol)

instance ( KnownSymbol serv
         , KnownSymbol meth

           -- Serialization
         , NFData (Input  (CborRpc serv meth))
         , NFData (Output (CborRpc serv meth))

           -- Debugging constraints
         , Show (Input  (CborRpc serv meth))
         , Show (Output (CborRpc serv meth))
         , Show (RequestMetadata (CborRpc serv meth))
         , Show (ResponseInitialMetadata (CborRpc serv meth))
         , Show (ResponseTrailingMetadata (CborRpc serv meth))
         ) => IsRPC (CborRpc serv meth) where
  rpcContentType _ = defaultRpcContentType "cbor"
  rpcServiceName _ = BS.Char8.pack $ symbolVal (Proxy @serv)
  rpcMethodName  _ = BS.Char8.pack $ symbolVal (Proxy @meth)
  rpcMessageType _ = Nothing

instance ( IsRPC (CborRpc serv meth)

           -- Serialization constraints
         , Serialise (Input  (CborRpc serv meth))
         , Serialise (Output (CborRpc serv meth))

           -- Metadata constraints
         , BuildMetadata (RequestMetadata          (CborRpc serv meth))
         , ParseMetadata (ResponseInitialMetadata  (CborRpc serv meth))
         , ParseMetadata (ResponseTrailingMetadata (CborRpc serv meth))
         ) => SupportsClientRpc (CborRpc serv meth) where
  rpcSerializeInput    _ = serialise
  rpcDeserializeOutput _ = deserialiseExactly

instance ( IsRPC (CborRpc serv meth)

           -- Serialization constraints
         , Serialise (Input  (CborRpc serv meth))
         , Serialise (Output (CborRpc serv meth))

           -- Metadata constraints
         , ParseMetadata (RequestMetadata          (CborRpc serv meth))
         , BuildMetadata (ResponseInitialMetadata  (CborRpc serv meth))
         , BuildMetadata (ResponseTrailingMetadata (CborRpc serv meth))
         ) => SupportsServerRpc (CborRpc serv meth) where
  rpcDeserializeInput _ = deserialiseExactly
  rpcSerializeOutput  _ = serialise

-- | For CBOR protocol we do not check communication protocols
instance ValidStreamingType styp
      => SupportsStreamingType (CborRpc serv meth) styp

-- | Decode the CBOR-encoded value in the given 'Lazy.ByteString'
--
-- Fails if the decoder fails or if there are any bytes leftover after a
-- successful decode.
deserialiseExactly :: Serialise a => Lazy.ByteString -> Either String a
deserialiseExactly bs =
    case CBOR.deserialiseFromBytes decode bs of
      Left err ->
        Left $ displayException err
      Right (unconsumed, a) ->
        if BS.Lazy.null unconsumed then
          Right a
        else
          Left "Not all bytes consumed"
