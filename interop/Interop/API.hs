module Interop.API (
    -- * Endpoints
    EmptyCall
  , UnaryCall
  , StreamingInputCall
  , StreamingOutputCall
  , FullDuplexCall
  , UnimplementedCall

    -- * Re-exports
  , module Network.GRPC.Common.Protobuf
  , module Proto.Empty
  , module Proto.Messages
  , module Proto.Test
  ) where

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