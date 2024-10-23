module Proto.API.Fileserver (
    module Proto.Fileserver
    -- * Metadata
  , DownloadStart(..)
  , DownloadDone(..)
  ) where

import Control.Monad.Catch (throwM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Text.Read (readMaybe)

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.Fileserver
import Data.Time (UTCTime)

{-------------------------------------------------------------------------------
  Metadata
-------------------------------------------------------------------------------}

type instance RequestMetadata          (Protobuf Fileserver "download") = NoMetadata
type instance ResponseInitialMetadata  (Protobuf Fileserver "download") = DownloadStart
type instance ResponseTrailingMetadata (Protobuf Fileserver "download") = DownloadDone

data DownloadStart = DownloadStart {
      downloadSize    :: Integer
    , downloadModTime :: Maybe UTCTime
    }
  deriving stock (Show)

data DownloadDone = DownloadDone {
      downloadHash :: ByteString
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Serialization (server-side)
-------------------------------------------------------------------------------}

instance BuildMetadata DownloadStart where
  buildMetadata DownloadStart{downloadSize, downloadModTime} = [
        CustomMetadata "download-size"     $ C8.pack (show downloadSize)
      , CustomMetadata "download-mod-time" $ C8.pack (show downloadModTime)
      ]

instance BuildMetadata DownloadDone where
  buildMetadata DownloadDone{downloadHash} = [
        CustomMetadata "download-hash-bin" downloadHash
      ]

instance StaticMetadata DownloadDone where
  metadataHeaderNames _ = ["download-hash-bin", "download-mod-time"]

{-------------------------------------------------------------------------------
  Deserialization (client-side)

  NOTE: We intentionally do not parse @download-mod-time@ here; see tutorial.
-------------------------------------------------------------------------------}

instance ParseMetadata DownloadStart where
  parseMetadata md =
      case md of
        [CustomMetadata "download-size" value]
          | Just downloadSize <- readMaybe (C8.unpack value)
          -> return $ DownloadStart{downloadSize, downloadModTime = Nothing}
        _otherwise
          -> throwM $ UnexpectedMetadata md

instance ParseMetadata DownloadDone where
  parseMetadata md =
      case md of
        [CustomMetadata "download-hash-bin" downloadHash]
          -> return $ DownloadDone{downloadHash}
        _otherwise
          -> throwM $ UnexpectedMetadata md

