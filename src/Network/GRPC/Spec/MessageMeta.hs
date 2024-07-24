-- | Information about messages
module Network.GRPC.Spec.MessageMeta (
    OutboundMeta(..)
  , InboundMeta(..)
  ) where

import Data.Default
import Data.Word

{-------------------------------------------------------------------------------
  Outbound messages
-------------------------------------------------------------------------------}

data OutboundMeta = OutboundMeta {
      -- | Enable compression for this message
      --
      -- Even if enabled, compression will only be used if this results in a
      -- smaller message.
      outboundEnableCompression :: Bool
    }
  deriving stock (Show)

instance Default OutboundMeta where
  def = OutboundMeta {
        outboundEnableCompression = True
      }

{-------------------------------------------------------------------------------
  Inbound messages
-------------------------------------------------------------------------------}

data InboundMeta = InboundMeta {
      -- | Size of the message in compressed form, /if/ it was compressed
      inboundCompressedSize :: Maybe Word32

      -- | Size of the message in uncompressed (but still serialized) form
    , inboundUncompressedSize :: Word32
    }
  deriving stock (Show)

