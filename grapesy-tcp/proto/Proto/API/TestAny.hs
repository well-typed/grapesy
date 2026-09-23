{-# OPTIONS_GHC -Wno-orphans #-}

module Proto.API.TestAny (
    Reverse

    -- * Re-exports
  , module Proto.TestAny
  ) where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.TestAny

{-------------------------------------------------------------------------------
  TestAnyService
-------------------------------------------------------------------------------}

type Reverse = Protobuf TestAnyService "reverse"

type instance RequestMetadata          (Protobuf TestAnyService meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf TestAnyService meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf TestAnyService meth) = NoMetadata
