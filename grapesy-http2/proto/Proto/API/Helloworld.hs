{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Proto.API.Helloworld (
    -- * Greeter
    SayHello
  , SayHelloStreamReply
  , SayHelloBidiStream

    -- ** Metadata
  , SayHelloMetadata(..)

    -- * Re-exports
  , module Proto.Helloworld
  ) where

import Control.Monad.Catch
import Data.ByteString qualified as Strict
import GHC.TypeLits

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.Helloworld

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