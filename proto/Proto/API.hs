{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Proto.API (
    -- * Greeter
    SayHello
  , SayHelloStreamReply
  , SayHelloBidiStream

    -- ** Metadata
  , SayHelloMetadata(..)

    -- * RouteGuide
  , GetFeature
  , ListFeatures
  , RecordRoute
  , RouteChat

    -- * Ping
  , Ping

    -- * Re-exports
  , module Proto.Helloworld
  , module Proto.RouteGuide
  ) where

import Control.Monad.Catch
import Data.ByteString qualified as Strict
import GHC.TypeLits

import Network.GRPC.Common
import Network.GRPC.Common.Binary
import Network.GRPC.Common.Protobuf

import Proto.Helloworld
import Proto.RouteGuide

{-------------------------------------------------------------------------------
  Greeter
-------------------------------------------------------------------------------}

type SayHello            = Protobuf Greeter "sayHello"
type SayHelloStreamReply = Protobuf Greeter "sayHelloStreamReply"
type SayHelloBidiStream  = Protobuf Greeter "sayHelloBidiStream"

type instance RequestMetadata          (Protobuf Greeter meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf Greeter meth) = GreeterResponseInitialMetadata meth
type instance ResponseTrailingMetadata (Protobuf Greeter meth) = NoMetadata

{-------------------------------------------------------------------------------
  .. metadata
-------------------------------------------------------------------------------}

type family GreeterResponseInitialMetadata (meth :: Symbol) where
  GreeterResponseInitialMetadata "sayHelloStreamReply" = SayHelloMetadata
  GreeterResponseInitialMetadata meth                  = NoMetadata

data SayHelloMetadata = SayHelloMetadata (Maybe Strict.ByteString)
  deriving (Show)

instance BuildMetadata SayHelloMetadata where
  buildMetadata (SayHelloMetadata mVal) = concat [
        [ CustomMetadata "initial-md" val
        | Just val <- [mVal]
        ]
      ]

instance ParseMetadata SayHelloMetadata where
  parseMetadata headers =
      case headers of
        [] ->
          return $ SayHelloMetadata $ Nothing
        [md] | customMetadataName md == "initial-md" ->
          return $ SayHelloMetadata $ Just (customMetadataValue md)
        _otherwise ->
          throwM $ UnexpectedMetadata headers

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

{-------------------------------------------------------------------------------
  Ping
-------------------------------------------------------------------------------}

type Ping = RawRpc "Ping" "ping"

type instance RequestMetadata          (RawRpc "Ping" meth) = NoMetadata
type instance ResponseInitialMetadata  (RawRpc "Ping" meth) = NoMetadata
type instance ResponseTrailingMetadata (RawRpc "Ping" meth) = NoMetadata
