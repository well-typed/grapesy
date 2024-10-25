module Network.GRPC.Spec.Serialization.Status (
    buildGrpcStatus
  , parseGrpcStatus
  ) where

import Network.GRPC.Spec

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

-- | Translate 'GrpcStatus to numerical code
--
-- See <https://grpc.github.io/grpc/core/md_doc_statuscodes.html>
buildGrpcStatus :: GrpcStatus -> Word
buildGrpcStatus  GrpcOk                            =  0
buildGrpcStatus (GrpcError GrpcCancelled)          =  1
buildGrpcStatus (GrpcError GrpcUnknown)            =  2
buildGrpcStatus (GrpcError GrpcInvalidArgument)    =  3
buildGrpcStatus (GrpcError GrpcDeadlineExceeded)   =  4
buildGrpcStatus (GrpcError GrpcNotFound)           =  5
buildGrpcStatus (GrpcError GrpcAlreadyExists)      =  6
buildGrpcStatus (GrpcError GrpcPermissionDenied)   =  7
buildGrpcStatus (GrpcError GrpcResourceExhausted)  =  8
buildGrpcStatus (GrpcError GrpcFailedPrecondition) =  9
buildGrpcStatus (GrpcError GrpcAborted)            = 10
buildGrpcStatus (GrpcError GrpcOutOfRange)         = 11
buildGrpcStatus (GrpcError GrpcUnimplemented)      = 12
buildGrpcStatus (GrpcError GrpcInternal)           = 13
buildGrpcStatus (GrpcError GrpcUnavailable)        = 14
buildGrpcStatus (GrpcError GrpcDataLoss)           = 15
buildGrpcStatus (GrpcError GrpcUnauthenticated)    = 16

-- | Inverse to 'buildGrpcStatus'
parseGrpcStatus :: Word -> Maybe GrpcStatus
parseGrpcStatus  0 = Just $ GrpcOk
parseGrpcStatus  1 = Just $ GrpcError $ GrpcCancelled
parseGrpcStatus  2 = Just $ GrpcError $ GrpcUnknown
parseGrpcStatus  3 = Just $ GrpcError $ GrpcInvalidArgument
parseGrpcStatus  4 = Just $ GrpcError $ GrpcDeadlineExceeded
parseGrpcStatus  5 = Just $ GrpcError $ GrpcNotFound
parseGrpcStatus  6 = Just $ GrpcError $ GrpcAlreadyExists
parseGrpcStatus  7 = Just $ GrpcError $ GrpcPermissionDenied
parseGrpcStatus  8 = Just $ GrpcError $ GrpcResourceExhausted
parseGrpcStatus  9 = Just $ GrpcError $ GrpcFailedPrecondition
parseGrpcStatus 10 = Just $ GrpcError $ GrpcAborted
parseGrpcStatus 11 = Just $ GrpcError $ GrpcOutOfRange
parseGrpcStatus 12 = Just $ GrpcError $ GrpcUnimplemented
parseGrpcStatus 13 = Just $ GrpcError $ GrpcInternal
parseGrpcStatus 14 = Just $ GrpcError $ GrpcUnavailable
parseGrpcStatus 15 = Just $ GrpcError $ GrpcDataLoss
parseGrpcStatus 16 = Just $ GrpcError $ GrpcUnauthenticated
parseGrpcStatus _  = Nothing

