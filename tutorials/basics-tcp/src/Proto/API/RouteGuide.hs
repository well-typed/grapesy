{-# OPTIONS_GHC -Wno-orphans #-}

module Proto.API.RouteGuide (
    module Proto.RouteGuide
  ) where

import Data.ProtoLens.Labels ()

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.RouteGuide

{-------------------------------------------------------------------------------
  Metadata
-------------------------------------------------------------------------------}

type instance RequestMetadata          (Protobuf RouteGuide meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf RouteGuide meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf RouteGuide meth) = NoMetadata
