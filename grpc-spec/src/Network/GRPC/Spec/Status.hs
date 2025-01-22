module Network.GRPC.Spec.Status (
    -- * GRPC status
    GrpcStatus(..)
  , GrpcError(..)
  , fromGrpcStatus
  , fromGrpcError
  , toGrpcStatus
  , toGrpcError
    -- * Exceptions
  , GrpcException(..)
  , throwGrpcError
    -- * Details
  , Status
  ) where

import Control.Exception
import Data.ByteString qualified as Strict (ByteString)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

import Network.GRPC.Spec.CustomMetadata.Raw (CustomMetadata)

import Proto.Status

{-------------------------------------------------------------------------------
  gRPC status
-------------------------------------------------------------------------------}

-- | gRPC status
--
-- Defined in <https://github.com/grpc/grpc/blob/master/doc/statuscodes.md>.
data GrpcStatus =
    GrpcOk
  | GrpcError GrpcError
  deriving stock (Show, Eq, Generic)

-- | gRPC error code
--
-- This is a subset of the gRPC status codes. See 'GrpcStatus'.
data GrpcError =
    -- | Cancelled
    --
    -- The operation was cancelled, typically by the caller.
    GrpcCancelled

    -- | Unknown error
    --
    -- For example, this error may be returned when a @Status@ value received
    -- from another address space belongs to an error space that is not known in
    -- this address space. Also errors raised by APIs that do not return enough
    -- error information may be converted to this error.
  | GrpcUnknown

    -- | Invalid argument
    --
    -- The client specified an invalid argument. Note that this differs from
    -- 'GrpcFailedPrecondition': 'GrpcInvalidArgument' indicates arguments that
    -- are problematic regardless of the state of the system (e.g., a malformed
    -- file name).
  | GrpcInvalidArgument

    -- | Deadline exceeded
    --
    -- The deadline expired before the operation could complete. For operations
    -- that change the state of the system, this error may be returned even if
    -- the operation has completed successfully. For example, a successful
    -- response from a server could have been delayed long.
  | GrpcDeadlineExceeded

    -- | Not found
    --
    -- Some requested entity (e.g., file or directory) was not found.
    --
    -- Note to server developers: if a request is denied for an entire class of
    -- users, such as gradual feature rollout or undocumented allowlist,
    -- 'GrpcNotFound' may be used.
    --
    -- If a request is denied for some users within a class of users, such as
    -- user-based access control, 'GrpcPermissionDenied' must be used.
  | GrpcNotFound

    -- | Already exists
    --
    -- The entity that a client attempted to create (e.g., file or directory)
    -- already exists.
  | GrpcAlreadyExists

    -- | Permission denied
    --
    -- The caller does not have permission to execute the specified operation.
    --
    -- * 'GrpcPermissionDenied' must not be used for rejections caused by
    --   exhausting some resource (use 'GrpcResourceExhausted' instead for those
    --   errors).
    -- * 'GrpcPermissionDenied' must not be used if the caller can not be
    --   identified (use 'GrpcUnauthenticated' instead for those errors).
    --
    -- This error code does not imply the request is valid or the requested
    -- entity exists or satisfies other pre-conditions.
  | GrpcPermissionDenied

    -- | Resource exhausted
    --
    -- Some resource has been exhausted, perhaps a per-user quota, or perhaps
    -- the entire file system is out of space.
  | GrpcResourceExhausted

    -- | Failed precondition
    --
    -- The operation was rejected because the system is not in a state required
    -- for the operation's execution. For example, the directory to be deleted
    -- is non-empty, an rmdir operation is applied to a non-directory, etc.
    --
    -- Service implementors can use the following guidelines to decide between
    -- 'GrpcFailedPrecondition', 'GrpcAborted', and 'GrpcUnavailable':
    --
    -- (a) Use 'GrpcUnavailable' if the client can retry just the failing call.
    -- (b) Use 'GrpcAborted' if the client should retry at a higher level (e.g.,
    --     when a client-specified test-and-set fails, indicating the client
    --     should restart a read-modify-write sequence).
    -- (c) Use `GrpcFailedPrecondition` if the client should not retry until the
    --     system state has been explicitly fixed. E.g., if an @rmdir@ fails
    --     because the directory is non-empty, 'GrpcFailedPrecondition' should
    --     be returned since the client should not retry unless the files are
    --     deleted from the directory.
  | GrpcFailedPrecondition

    -- | Aborted
    --
    -- The operation was aborted, typically due to a concurrency issue such as a
    -- sequencer check failure or transaction abort. See the guidelines above
    -- for deciding between 'GrpcFailedPrecondition', 'GrpcAborted', and
    -- 'GrpcUnavailable'.
  | GrpcAborted

    -- | Out of range
    --
    -- The operation was attempted past the valid range. E.g., seeking or
    -- reading past end-of-file.
    --
    -- Unlike 'GrpcInvalidArgument', this error indicates a problem that may be
    -- fixed if the system state changes. For example, a 32-bit file system will
    -- generate 'GrpcInvalidArgument' if asked to read at an offset that is not
    -- in the range @[0, 2^32-1]@, but it will generate 'GrpcOutOfRange' if
    -- asked to read from an offset past the current file size.
    --
    -- There is a fair bit of overlap between 'GrpcFailedPrecondition' and
    -- 'GrpcOutOfRange'. We recommend using 'GrpcOutOfRange' (the more specific
    -- error) when it applies so that callers who are iterating through a space
    -- can easily look for an 'GrpcOutOfRange' error to detect when they are
    -- done.
  | GrpcOutOfRange

    -- | Unimplemented
    --
    -- The operation is not implemented or is not supported/enabled in this
    -- service.
  | GrpcUnimplemented

    -- | Internal errors
    --
    -- This means that some invariants expected by the underlying system have
    -- been broken. This error code is reserved for serious errors.
  | GrpcInternal

    -- | Unavailable
    --
    -- The service is currently unavailable. This is most likely a transient
    -- condition, which can be corrected by retrying with a backoff. Note that
    -- it is not always safe to retry non-idempotent operations.
  | GrpcUnavailable

    -- | Data loss
    --
    -- Unrecoverable data loss or corruption.
  | GrpcDataLoss

    -- | Unauthenticated
    --
    -- The request does not have valid authentication credentials for the
    -- operation.
  | GrpcUnauthenticated
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Status codes
-------------------------------------------------------------------------------}

-- | Translate 'GrpcStatus' to numerical status code
--
-- See <https://grpc.github.io/grpc/core/md_doc_statuscodes.html>
fromGrpcStatus :: GrpcStatus -> Word
fromGrpcStatus  GrpcOk         =  0
fromGrpcStatus (GrpcError err) = fromGrpcError err

-- | Translate 'GrpcError' to numerical status code
--
-- See also 'fromGrpcStatus'
fromGrpcError :: GrpcError -> Word
fromGrpcError GrpcCancelled          =  1
fromGrpcError GrpcUnknown            =  2
fromGrpcError GrpcInvalidArgument    =  3
fromGrpcError GrpcDeadlineExceeded   =  4
fromGrpcError GrpcNotFound           =  5
fromGrpcError GrpcAlreadyExists      =  6
fromGrpcError GrpcPermissionDenied   =  7
fromGrpcError GrpcResourceExhausted  =  8
fromGrpcError GrpcFailedPrecondition =  9
fromGrpcError GrpcAborted            = 10
fromGrpcError GrpcOutOfRange         = 11
fromGrpcError GrpcUnimplemented      = 12
fromGrpcError GrpcInternal           = 13
fromGrpcError GrpcUnavailable        = 14
fromGrpcError GrpcDataLoss           = 15
fromGrpcError GrpcUnauthenticated    = 16

-- | Inverse to 'fromGrpcStatus'
toGrpcStatus :: Word -> Maybe GrpcStatus
toGrpcStatus 0 = Just $ GrpcOk
toGrpcStatus s = GrpcError <$> toGrpcError s

-- | Inverse to 'fromGrpcError'
toGrpcError :: Word -> Maybe GrpcError
toGrpcError  1 = Just $ GrpcCancelled
toGrpcError  2 = Just $ GrpcUnknown
toGrpcError  3 = Just $ GrpcInvalidArgument
toGrpcError  4 = Just $ GrpcDeadlineExceeded
toGrpcError  5 = Just $ GrpcNotFound
toGrpcError  6 = Just $ GrpcAlreadyExists
toGrpcError  7 = Just $ GrpcPermissionDenied
toGrpcError  8 = Just $ GrpcResourceExhausted
toGrpcError  9 = Just $ GrpcFailedPrecondition
toGrpcError 10 = Just $ GrpcAborted
toGrpcError 11 = Just $ GrpcOutOfRange
toGrpcError 12 = Just $ GrpcUnimplemented
toGrpcError 13 = Just $ GrpcInternal
toGrpcError 14 = Just $ GrpcUnavailable
toGrpcError 15 = Just $ GrpcDataLoss
toGrpcError 16 = Just $ GrpcUnauthenticated
toGrpcError _  = Nothing

{-------------------------------------------------------------------------------
  gRPC exceptions
-------------------------------------------------------------------------------}

-- | Server indicated a gRPC error
--
-- For the common case where you just want to set 'grpcError', you can use
-- 'throwGrpcError'.
data GrpcException = GrpcException {
      grpcError          :: GrpcError
    , grpcErrorMessage   :: Maybe Text
    , grpcErrorDetails   :: Maybe Strict.ByteString
    , grpcErrorMetadata  :: [CustomMetadata]
    }
  deriving stock (Show, Eq)

instance Exception GrpcException where
  displayException GrpcException{
                       grpcError
                     , grpcErrorMessage
                     , grpcErrorDetails
                     , grpcErrorMetadata
                     } = (intercalate "\n" . concat) [
        [ concat [
            "gRPC exception "
          , show grpcError
          , " ("
          , show (fromGrpcError grpcError)
          , ")"
          ]
        ]
      , [ intercalate "\n" $
              "Error message:"
            : (map ("| " ++) . lines $ Text.unpack msg)
        | Just msg <- [grpcErrorMessage]
        ]
      , [ "Additional details are available (see 'grpcErrorDetails')."
        | Just _details <- [grpcErrorDetails]
        ]
      , [ show md
        | md <- grpcErrorMetadata
        ]
      ]


-- | Convenience function to throw an t'GrpcException' with the specified error
throwGrpcError :: GrpcError -> IO a
throwGrpcError grpcError = throwIO $ GrpcException {
      grpcError
    , grpcErrorMessage  = Nothing
    , grpcErrorDetails  = Nothing
    , grpcErrorMetadata = []
    }
