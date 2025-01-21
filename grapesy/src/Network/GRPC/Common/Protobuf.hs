{-# LANGUAGE OverloadedLabels #-}

-- | Common functionality for working with Protobuf
module Network.GRPC.Common.Protobuf (
    Protobuf
  , Proto(..)
  , getProto

    -- * Exceptions
  , ProtobufError(..)
  , throwProtobufError
  , throwProtobufErrorHom
  , toProtobufError
  , toProtobufErrorHom

    -- * Re-exports
    -- ** "Data.Function"
  , (&)
    -- ** "Control.Lens"
  , (.~)
  , (^.)
  , (%~)
    -- ** "Data.ProtoLens"
  , StreamingType(..)
  , HasField(..)
  , FieldDefault(..)
  , Message(defMessage)
  ) where

import Control.Exception
import Control.Lens ((.~), (^.), (%~))
import Control.Monad
import Control.Monad.Except
import Data.Bifunctor
import Data.Function ((&))
import Data.Int
import Data.Maybe (fromMaybe)
import Data.ProtoLens.Field (HasField(..))
import Data.ProtoLens.Labels () -- provides instances for OverloadedLabels
import Data.ProtoLens.Message (FieldDefault(..), Message(defMessage))
import Data.Text (Text)

import Network.GRPC.Common.Protobuf.Any (Any)
import Network.GRPC.Common.Protobuf.Any qualified as Any
import Network.GRPC.Spec
import Network.GRPC.Spec.Serialization

{-------------------------------------------------------------------------------
  Protobuf-specific errors
-------------------------------------------------------------------------------}

-- | gRPC exception with protobuf-specific error details
--
-- See also 'Status' and @google.rpc.Status@.
data ProtobufError a = ProtobufError {
      protobufErrorCode     :: GrpcError
    , protobufErrorMessage  :: Maybe Text
    , protobufErrorDetails  :: [a]
    }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Throw 'GrpcException' with Protobuf-specific details
throwProtobufError :: ProtobufError (Proto Any) -> IO x
throwProtobufError ProtobufError{
                       protobufErrorCode
                     , protobufErrorMessage
                     , protobufErrorDetails
                     } = throwIO $ GrpcException {
      grpcError         = protobufErrorCode
    , grpcErrorMessage  = protobufErrorMessage
    , grpcErrorDetails  = Just $ buildStatus status
    , grpcErrorMetadata = []
    }
  where
    status :: Proto Status
    status =
        defMessage
          & #code    .~ fromIntegral (fromGrpcError protobufErrorCode)
          & #message .~ fromMaybe fieldDefault protobufErrorMessage
          & #details .~ protobufErrorDetails

-- | Variation of 'throwProtobufError' for a homogenous list of details
--
-- The @google.rpc.Status@ message uses the Protobuf 'Any' type to store a
-- heterogenous list of details. In case that all elements in this list are
-- actually of the /same/ type, we can provide a simpler API.
throwProtobufErrorHom :: Message a => ProtobufError (Proto a) -> IO x
throwProtobufErrorHom = throwProtobufError . fmap Any.pack

-- | Construct 'ProtobufError' by parsing 'grpcErrorDetails' as 'Status'
--
-- See also 'throwProtobufError'.
toProtobufError :: GrpcException -> Either String (ProtobufError (Proto Any))
toProtobufError err =
    case grpcErrorDetails err of
      Nothing ->
        return ProtobufError{
            protobufErrorCode    = grpcError err
          , protobufErrorMessage = grpcErrorMessage err
          , protobufErrorDetails = []
          }
      Just statusEnc -> do
        status :: Proto Status <- parseStatus statusEnc
        protobufErrorCode <- checkErrorCode (status ^. #code)
        return ProtobufError{
            protobufErrorCode
          , protobufErrorMessage = constructErrorMessage (status ^. #message)
          , protobufErrorDetails = status ^. #details
          }
  where
    -- The gRPC specification mandates
    --
    -- > If [Status-Details] contains a status code field, it MUST NOT
    -- > contradict the Status header. The consumer MUST verify this
    -- > requirement.
    --
    -- We cannot do this check without knowing the format of the status details,
    -- so we do it here for the specific case of Protobuf. Since we cannot
    -- distinguish between an absent field and a field with a default value in
    -- Protobuf, we add a special case for this.
    checkErrorCode :: Int32 -> Either String GrpcError
    checkErrorCode statusCode
      | statusCode == fieldDefault
      = return $ grpcError err

      | fromGrpcError (grpcError err) == fromIntegral statusCode
      = return $ grpcError err

      | otherwise
      = throwError $ "'Status.code' does not match 'grpc-status'"

    -- If the Status-Details does not contain a message (that is, the field is
    -- at its default value), we fall back to the status message in the
    -- exception itself (if any).
    constructErrorMessage :: Text -> Maybe Text
    constructErrorMessage msg =
        if msg == fieldDefault
          then grpcErrorMessage err
          else Just msg

-- | Variation of 'toProtobufError' for a homogenous list of details
toProtobufErrorHom :: forall a.
     Message a
  => GrpcException -> Either String (ProtobufError (Proto a))
toProtobufErrorHom = traverse aux <=< toProtobufError
  where
    aux :: Proto Any -> Either String (Proto a)
    aux = first show . Any.unpack
