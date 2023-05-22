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
  , protocolException
  ) where

import Data.Text (Text)
import Control.Exception

import Network.GRPC.Spec
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.RPC
import Control.Monad.Catch

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

grpcExceptionFromTrailers :: Trailers -> Maybe GrpcException
grpcExceptionFromTrailers trailers =
    case trailerGrpcStatus trailers of
      GrpcOk        -> Nothing
      GrpcError err -> Just GrpcException{
          grpcError         = err
        , grpcErrorMessage  = trailerGrpcMessage trailers
        , grpcErrorMetadata = trailerMetadata    trailers
        }

grpcExceptionToTrailers ::  GrpcException -> Trailers
grpcExceptionToTrailers err = Trailers{
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

-- | Throw protocol exception
--
-- This is a convenience function, primarily to aid type inference.
protocolException ::
     (MonadThrow m, IsRPC rpc)
  => rpc -> ProtocolException rpc -> m a
protocolException _ = throwM

