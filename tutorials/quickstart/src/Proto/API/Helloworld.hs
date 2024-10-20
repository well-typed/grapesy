module Proto.API.Helloworld (
    module Proto.Helloworld
  ) where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.Helloworld

{-------------------------------------------------------------------------------
  Metadata
-------------------------------------------------------------------------------}

type instance RequestMetadata          (Protobuf Greeter meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf Greeter meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Greeter meth) = NoMetadata
