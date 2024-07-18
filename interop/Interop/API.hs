{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Interop.API (
    -- * TestService

    -- ** Endpoints
    EmptyCall
  , UnaryCall
  , StreamingInputCall
  , StreamingOutputCall
  , FullDuplexCall
  , UnimplementedCall

    -- ** Metadata
  , InteropReqMeta(..)
  , InteropRespInitMeta(..)
  , InteropRespTrailMeta(..)

    -- * UnimplementedService
  , UnimplementedServiceCall

    -- * Re-exports
  , module Proto.Empty
  , module Proto.Messages
  , module Proto.Test
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.State (StateT, execStateT, modify)

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.Empty
import Proto.Messages
import Proto.Test

{-------------------------------------------------------------------------------
  Endpoints
-------------------------------------------------------------------------------}

type EmptyCall           = Protobuf TestService "emptyCall"
type UnaryCall           = Protobuf TestService "unaryCall"
type StreamingInputCall  = Protobuf TestService "streamingInputCall"
type StreamingOutputCall = Protobuf TestService "streamingOutputCall"
type FullDuplexCall      = Protobuf TestService "fullDuplexCall"
type UnimplementedCall   = Protobuf TestService "unimplementedCall"

{-------------------------------------------------------------------------------
  Metadata
-------------------------------------------------------------------------------}

type instance RequestMetadata          (Protobuf TestService meth) = InteropReqMeta
type instance ResponseInitialMetadata  (Protobuf TestService meth) = InteropRespInitMeta
type instance ResponseTrailingMetadata (Protobuf TestService meth) = InteropRespTrailMeta

data InteropReqMeta = InteropReqMeta {
      -- | Header we expect the server to include in the initial metadata
      interopExpectInit :: Maybe Strict.ByteString

      -- | Header we expect the server to include in the trailng metadata
    , interopExpectTrail :: Maybe Strict.ByteString
    }
  deriving (Show, Eq)

newtype InteropRespInitMeta = InteropRespInitMeta {
      -- | Metadata the server /actually/ included in the initial metadata
      --
      -- See also 'interopExpectInit'
      interopActualInit :: Maybe Strict.ByteString
    }
  deriving (Show, Eq)

newtype InteropRespTrailMeta = InteropRespTrailMeta {
      -- | Metadata the server /actually/ included in the trailing metadata
      --
      -- See also 'interopExpectTrail'
      interopActualTrail :: Maybe Strict.ByteString
    }
  deriving (Show, Eq)

grpcTestEchoInitial :: HeaderName
grpcTestEchoInitial = "x-grpc-test-echo-initial"

grpcTestEchoTrailingBin :: HeaderName
grpcTestEchoTrailingBin = "x-grpc-test-echo-trailing-bin"

{-------------------------------------------------------------------------------
  Client instances
-------------------------------------------------------------------------------}

instance Default InteropReqMeta where
  def = InteropReqMeta {
        interopExpectInit  = Nothing
      , interopExpectTrail = Nothing
      }

instance BuildMetadata InteropReqMeta where
  buildMetadata md = concat [
        [ CustomMetadata grpcTestEchoInitial val
        | Just val <- [interopExpectInit md]
        ]
      , [ CustomMetadata grpcTestEchoTrailingBin val
        | Just val <- [interopExpectTrail md]
        ]
      ]

instance ParseMetadata InteropRespInitMeta where
  parseMetadata headers =
      case headers of
        [] ->
          return $ InteropRespInitMeta $ Nothing
        [md] | customMetadataName md == grpcTestEchoInitial ->
          return $ InteropRespInitMeta $ Just (customMetadataValue md)
        _otherwise ->
          throwM $ UnexpectedMetadata headers

instance ParseMetadata InteropRespTrailMeta where
  parseMetadata headers =
      case headers of
        [] ->
          return $ InteropRespTrailMeta $ Nothing
        [md] | customMetadataName md == grpcTestEchoTrailingBin ->
          return $ InteropRespTrailMeta $ Just (customMetadataValue md)
        _otherwise ->
          throwM $ UnexpectedMetadata headers

{-------------------------------------------------------------------------------
  Server instances
-------------------------------------------------------------------------------}

instance Default InteropRespInitMeta where
  def = InteropRespInitMeta Nothing

instance Default InteropRespTrailMeta where
  def = InteropRespTrailMeta Nothing

instance ParseMetadata InteropReqMeta where
  parseMetadata = flip execStateT def . mapM go
    where
      go :: MonadThrow m => CustomMetadata -> StateT InteropReqMeta m ()
      go md
        | customMetadataName md == grpcTestEchoInitial
        = modify $ \x -> x{interopExpectInit = Just $ customMetadataValue md}

        | customMetadataName md == grpcTestEchoTrailingBin
        = modify $ \x -> x{interopExpectTrail = Just $ customMetadataValue md}

        | otherwise
        = throwM $ UnexpectedMetadata [md]

instance BuildMetadata InteropRespInitMeta where
  buildMetadata md = concat [
        [ CustomMetadata grpcTestEchoInitial val
        | Just val <- [interopActualInit md]
        ]
      ]

instance BuildMetadata InteropRespTrailMeta where
  buildMetadata md = concat [
        [ CustomMetadata grpcTestEchoTrailingBin val
        | Just val <- [interopActualTrail md]
        ]
      ]

instance StaticMetadata InteropRespTrailMeta where
  metadataHeaderNames _ = [grpcTestEchoTrailingBin]

{-------------------------------------------------------------------------------
  UnimplementedService service
-------------------------------------------------------------------------------}

type UnimplementedServiceCall = Protobuf UnimplementedService "unimplementedCall"

type instance RequestMetadata          (Protobuf UnimplementedService meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf UnimplementedService meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf UnimplementedService meth) = NoMetadata
