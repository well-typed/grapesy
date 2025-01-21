module Network.GRPC.Spec.Serialization.Status (
    buildStatus
  , parseStatus
  ) where

import Data.ByteString qualified as Strict (ByteString)

import Network.GRPC.Spec
import Network.GRPC.Spec.Util.Protobuf

buildStatus :: Proto Status -> Strict.ByteString
buildStatus = buildStrict

parseStatus :: Strict.ByteString -> Either String (Proto Status)
parseStatus = parseStrict
