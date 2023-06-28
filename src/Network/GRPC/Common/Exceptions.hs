-- | Exceptions
--
-- Intended for unqualified import
module Network.GRPC.Common.Exceptions (
    -- * gRPC exception
    GrpcException(..)
  , grpcExceptionFromTrailers
  , grpcExceptionToTrailers
    -- * Protocol exception
  , ProtocolException(..)
  ) where

import Control.Exception
import Data.Text (Text)

import Network.GRPC.Spec
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.RPC

{-------------------------------------------------------------------------------
  gRPC exception
-------------------------------------------------------------------------------}

-- | Server indicated a gRPC error
data GrpcException = GrpcException {
      grpcError         :: GrpcError
    , grpcErrorMessage  :: Maybe Text
    , grpcErrorMetadata :: [CustomMetadata]
    }
  deriving stock (Show)
  deriving anyclass (Exception)

grpcExceptionFromTrailers :: ProperTrailers -> Maybe GrpcException
grpcExceptionFromTrailers trailers =
    case trailerGrpcStatus trailers of
      GrpcOk        -> Nothing
      GrpcError err -> Just GrpcException{
          grpcError         = err
        , grpcErrorMessage  = trailerGrpcMessage trailers
        , grpcErrorMetadata = trailerMetadata    trailers
        }

grpcExceptionToTrailers ::  GrpcException -> ProperTrailers
grpcExceptionToTrailers err = ProperTrailers{
      trailerGrpcStatus   = GrpcError (grpcError err)
    , trailerGrpcMessage  = grpcErrorMessage     err
    , trailerMetadata     = grpcErrorMetadata    err
    }

{-------------------------------------------------------------------------------
  Protocol exception
-------------------------------------------------------------------------------}

-- | Protocol exception
--
-- A protocol exception arises when the client and the server disagree on the
-- sequence of inputs and outputs exchanged. This agreement might be part of a
-- formal specification such as Protobuf, or it might be implicit in the
-- implementation of a specific RPC.
data ProtocolException rpc =
    -- | We expected an input but got none
    TooFewInputs

    -- | We received an input when we expected no more inputs
  | TooManyInputs (Input rpc)

    -- | We expected an output, but got trailers instead
  | TooFewOutputs [CustomMetadata]

    -- | We expected trailers, but got an output instead
  | TooManyOutputs (Output rpc)

deriving instance IsRPC rpc => Show      (ProtocolException rpc)
deriving instance IsRPC rpc => Exception (ProtocolException rpc)
