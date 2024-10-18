module Proto.API.Helloworld (
    module Proto.Helloworld
  ) where

import Proto.Helloworld
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common

{-------------------------------------------------------------------------------
  Metadata
-------------------------------------------------------------------------------}

type instance RequestMetadata          (Protobuf Greeter meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf Greeter meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Greeter meth) = NoMetadata
