-- | Parts of the gRPC specification that are not HTTP2 specific.
--
-- Intended for unqualified import.
module Network.GRPC.Spec (
    -- * Call parameters
    CallParams(..)
    -- ** Timeouts
  , Timeout(..)
  , TimeoutValue(TimeoutValue, getTimeoutValue)
  , TimeoutUnit(..)
  , timeoutToMicro
    -- * Inputs (message sent to the peer)
  , RequestHeaders(..)
  , IsFinal(..)
    -- * Outputs (messages received from the peer)
  , ResponseHeaders(..)
  , ProperTrailers(..)
  , TrailersOnly(..)
    -- * GRPC status
  , GrpcStatus(..)
  , GrpcError(..)
  , fromGrpcStatus
  , toGrpcStatus
  ) where

import Control.Exception
import Data.Default
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Generics.SOP qualified as SOP
import GHC.Generics qualified as GHC
import GHC.Show

import Network.GRPC.Spec.Compression (CompressionId)
import Network.GRPC.Spec.CustomMetadata

{-------------------------------------------------------------------------------
  Requests
-------------------------------------------------------------------------------}

-- | RPC parameters that can be chosen on a per-call basis
data CallParams = CallParams {
      -- | Timeout
      --
      -- If Timeout is omitted a server should assume an infinite timeout.
      -- Client implementations are free to send a default minimum timeout based
      -- on their deployment requirements.
      callTimeout :: Maybe Timeout

      -- | Custom metadata
      --
      -- This is the metadata included in the request. (The server can include
      -- its own metadata in the response: see 'responseMetadata' and
      -- 'trailerMetadatda'.)
    , callRequestMetadata :: [CustomMetadata]
    }
  deriving stock (Show, Eq)

-- | Default 'CallParams'
instance Default CallParams where
  def = CallParams {
        callTimeout         = Nothing
      , callRequestMetadata = []
      }

{-------------------------------------------------------------------------------
  Timeouts
-------------------------------------------------------------------------------}

data Timeout = Timeout TimeoutUnit TimeoutValue
  deriving stock (Show, Eq)

-- | Positive integer with ASCII representation of at most 8 digits
newtype TimeoutValue = UnsafeTimeoutValue {
      getTimeoutValue :: Word
    }
  deriving newtype (Eq)

-- | 'Show' instance relies on the 'TimeoutValue' pattern synonym
instance Show TimeoutValue where
  showsPrec p (UnsafeTimeoutValue val) = showParen (p >= appPrec1) $
        showString "TimeoutValue "
      . showsPrec appPrec1 val

pattern TimeoutValue :: Word -> TimeoutValue
pattern TimeoutValue t <- UnsafeTimeoutValue t
  where
    TimeoutValue t
      | isValidTimeoutValue t = UnsafeTimeoutValue t
      | otherwise = error $ "invalid TimeoutValue: " ++ show t

{-# COMPLETE TimeoutValue #-}

isValidTimeoutValue :: Word -> Bool
isValidTimeoutValue t = length (show t) <= 8

data TimeoutUnit =
    Hour
  | Minute
  | Second
  | Millisecond
  | Microsecond
  | Nanosecond
  deriving stock (Show, Eq)

-- | Translate 'Timeout' to microseconds
--
-- For 'Nanosecond' timeout we round up.
timeoutToMicro :: Timeout -> Integer
timeoutToMicro = \case
    Timeout Hour        (TimeoutValue n) -> mult n $ 1 * 1_000 * 1_000 * 60 * 24
    Timeout Minute      (TimeoutValue n) -> mult n $ 1 * 1_000 * 1_000 * 60
    Timeout Second      (TimeoutValue n) -> mult n $ 1 * 1_000 * 1_000
    Timeout Millisecond (TimeoutValue n) -> mult n $ 1 * 1_000
    Timeout Microsecond (TimeoutValue n) -> mult n $ 1
    Timeout Nanosecond  (TimeoutValue n) -> nano n
  where
    mult :: Word -> Integer -> Integer
    mult n m = fromIntegral n * m

    nano :: Word -> Integer
    nano n = fromIntegral $
        mu + if n' == 0 then 0 else 1
      where
        (mu, n') = divMod n 1_000

{-------------------------------------------------------------------------------
  Inputs (message sent to the peer)
-------------------------------------------------------------------------------}

-- | Full set of call parameters required to construct the RPC call
--
-- This is constructed internally; it is not part of the public API.
data RequestHeaders = RequestHeaders {
      -- | Timeout
      requestTimeout :: Maybe Timeout

      -- | Custom metadata
    , requestMetadata :: [CustomMetadata]

      -- | Compression used for outgoing messages
    , requestCompression :: Maybe CompressionId

      -- | Accepted compression algorithms for incoming messages
      --
      -- @Maybe (NonEmpty ..)@ is perhaps a bit strange (why not just @[]@), but
      -- it emphasizes the specification: /if/ the header is present, it must be
      -- a non-empty list.
    , requestAcceptCompression :: Maybe (NonEmpty CompressionId)
    }
  deriving stock (Show, Eq)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Mark a input sent as final
data IsFinal = Final | NotFinal
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Outputs (messages received from the peer)
-------------------------------------------------------------------------------}

-- | Response headers
data ResponseHeaders = ResponseHeaders {
      responseCompression       :: Maybe CompressionId
    , responseAcceptCompression :: Maybe (NonEmpty CompressionId)
    , responseMetadata          :: [CustomMetadata]
    }
  deriving stock (Show, Eq)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Information sent by the peer after the final output
--
-- Response trailers are a
-- [HTTP2 concept](https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.3):
-- they are HTTP headers that are sent /after/ the content body. For example,
-- imagine the server is streaming a file that it's reading from disk; it could
-- use trailers to give the client an MD5 checksum when streaming is complete.
data ProperTrailers = ProperTrailers {
      trailerGrpcStatus  :: GrpcStatus
    , trailerGrpcMessage :: Maybe Text
    , trailerMetadata    :: [CustomMetadata]
    }
  deriving stock (Show, Eq)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Trailers sent in the gRPC Trailers-Only case
--
-- In the current version of the spec, the information in 'TrailersOnly' is
-- identical to the 'ProperTrailers' case (but they do require a slightly
-- different function to parse/unparse).
newtype TrailersOnly = TrailersOnly {
      getTrailersOnly :: ProperTrailers
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  gRPC status
-------------------------------------------------------------------------------}

-- | gRPC status
--
-- Defined in <https://github.com/grpc/grpc/blob/master/doc/statuscodes.md>.
data GrpcStatus =
    GrpcOk
  | GrpcError GrpcError
  deriving stock (Show, Eq)

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
    -- 'GrpcFailedPrecondition': 'GrpcInvalidArgumen'` indicates arguments that
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
    -- * 'GrpcPermissionDenoed' must not be used if the caller can not be
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
    -- 'GrpcFailedPrecondition', 'GrpcAborted', and 'GrpcUnvailable':
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
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

fromGrpcStatus :: GrpcStatus -> Word
fromGrpcStatus  GrpcOk                            =  0
fromGrpcStatus (GrpcError GrpcCancelled)          =  1
fromGrpcStatus (GrpcError GrpcUnknown)            =  2
fromGrpcStatus (GrpcError GrpcInvalidArgument)    =  3
fromGrpcStatus (GrpcError GrpcDeadlineExceeded)   =  4
fromGrpcStatus (GrpcError GrpcNotFound)           =  5
fromGrpcStatus (GrpcError GrpcAlreadyExists)      =  6
fromGrpcStatus (GrpcError GrpcPermissionDenied)   =  7
fromGrpcStatus (GrpcError GrpcResourceExhausted)  =  8
fromGrpcStatus (GrpcError GrpcFailedPrecondition) =  9
fromGrpcStatus (GrpcError GrpcAborted)            = 10
fromGrpcStatus (GrpcError GrpcOutOfRange)         = 11
fromGrpcStatus (GrpcError GrpcUnimplemented)      = 12
fromGrpcStatus (GrpcError GrpcInternal)           = 13
fromGrpcStatus (GrpcError GrpcUnavailable)        = 14
fromGrpcStatus (GrpcError GrpcDataLoss)           = 15
fromGrpcStatus (GrpcError GrpcUnauthenticated)    = 16

toGrpcStatus :: Word -> Maybe GrpcStatus
toGrpcStatus  0 = Just $ GrpcOk
toGrpcStatus  1 = Just $ GrpcError $ GrpcCancelled
toGrpcStatus  2 = Just $ GrpcError $ GrpcUnknown
toGrpcStatus  3 = Just $ GrpcError $ GrpcInvalidArgument
toGrpcStatus  4 = Just $ GrpcError $ GrpcDeadlineExceeded
toGrpcStatus  5 = Just $ GrpcError $ GrpcNotFound
toGrpcStatus  6 = Just $ GrpcError $ GrpcAlreadyExists
toGrpcStatus  7 = Just $ GrpcError $ GrpcPermissionDenied
toGrpcStatus  8 = Just $ GrpcError $ GrpcResourceExhausted
toGrpcStatus  9 = Just $ GrpcError $ GrpcFailedPrecondition
toGrpcStatus 10 = Just $ GrpcError $ GrpcAborted
toGrpcStatus 11 = Just $ GrpcError $ GrpcOutOfRange
toGrpcStatus 12 = Just $ GrpcError $ GrpcUnimplemented
toGrpcStatus 13 = Just $ GrpcError $ GrpcInternal
toGrpcStatus 14 = Just $ GrpcError $ GrpcUnavailable
toGrpcStatus 15 = Just $ GrpcError $ GrpcDataLoss
toGrpcStatus 16 = Just $ GrpcError $ GrpcUnauthenticated
toGrpcStatus _  = Nothing

