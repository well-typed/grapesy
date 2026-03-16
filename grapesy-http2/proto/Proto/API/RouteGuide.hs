{-# OPTIONS_GHC -Wno-orphans #-}

module Proto.API.RouteGuide (
    -- * RouteGuide
    GetFeature
  , ListFeatures
  , RecordRoute
  , RouteChat

    -- * Re-exports
  , module Proto.RouteGuide
  ) where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.RouteGuide

{-------------------------------------------------------------------------------
  RouteGuide
-------------------------------------------------------------------------------}

type GetFeature   = Protobuf RouteGuide "getFeature"
type ListFeatures = Protobuf RouteGuide "listFeatures"
type RecordRoute  = Protobuf RouteGuide "recordRoute"
type RouteChat    = Protobuf RouteGuide "routeChat"

type instance RequestMetadata          (Protobuf RouteGuide meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf RouteGuide meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf RouteGuide meth) = NoMetadata
