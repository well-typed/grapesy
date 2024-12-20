module Network.GRPC.Spec.CustomMetadata.Typed (
    -- * Map RPC to metadata
    RequestMetadata
  , ResponseInitialMetadata
  , ResponseTrailingMetadata
  , ResponseMetadata(..)
    -- * Serialization
  , BuildMetadata(..)
  , StaticMetadata(..)
  , ParseMetadata(..)
  , UnexpectedMetadata(..)
  , buildMetadataIO
  ) where

import Control.DeepSeq (force)
import Control.Exception
import Control.Monad.Catch
import Data.Kind
import Data.Proxy

import Network.GRPC.Spec.CustomMetadata.Raw

{-------------------------------------------------------------------------------
  Map RPC to metadata
-------------------------------------------------------------------------------}

-- | Metadata included in the request
--
-- Often you can give a blanket metadata definition for all methods in a
-- service. For example:
--
-- > type instance RequestMetadata          (Protobuf RouteGuide meth) = NoMetadata
-- > type instance ResponseInitialMetadata  (Protobuf RouteGuide meth) = NoMetadata
-- > type instance ResponseTrailingMetadata (Protobuf RouteGuide meth) = NoMetadata
--
-- If you want to give specific types of metadata for specific methods but not
-- for others, it can sometimes be useful to introduce an auxiliary closed type,
-- so that you can give a catch-all case. For example:
--
-- > type instance ResponseInitialMetadata (Protobuf Greeter meth) = GreeterResponseInitialMetadata meth
-- >
-- > type family GreeterResponseInitialMetadata (meth :: Symbol) where
-- >   GreeterResponseInitialMetadata "sayHelloStreamReply" = SayHelloMetadata
-- >   GreeterResponseInitialMetadata meth                  = NoMetadata
type family RequestMetadata (rpc :: k) :: Type

-- | Metadata included in the initial response
--
-- See 'RequestMetadata' for discussion.
type family ResponseInitialMetadata (rpc :: k) :: Type

-- | Metadata included in the response trailers
--
-- See 'RequestMetadata' for discussion.
type family ResponseTrailingMetadata (rpc :: k) :: Type

-- | Response metadata
--
-- It occassionally happens that we do not know if we should expect the initial
-- metadata from the server or the trailing metadata (when the server uses
-- Trailers-Only); for example, see
-- 'Network.GRPC.Client.recvResponseInitialMetadata'.
data ResponseMetadata rpc =
    ResponseInitialMetadata  (ResponseInitialMetadata  rpc)
  | ResponseTrailingMetadata (ResponseTrailingMetadata rpc)

deriving stock instance
     ( Show (ResponseInitialMetadata rpc)
     , Show (ResponseTrailingMetadata rpc)
     )
  => Show (ResponseMetadata rpc)

deriving stock instance
     ( Eq (ResponseInitialMetadata rpc)
     , Eq (ResponseTrailingMetadata rpc)
     )
  => Eq (ResponseMetadata rpc)

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

-- | Serialize metadata to custom metadata headers
class BuildMetadata a where
  buildMetadata :: a -> [CustomMetadata]

-- | Wrapper around 'buildMetadata' that catches any pure exceptions
--
-- These pure exceptions can arise when invalid headers are generated (for
-- example, ASCII headers with non-ASCII values).
buildMetadataIO :: BuildMetadata a => a -> IO [CustomMetadata]
buildMetadataIO = evaluate . force . buildMetadata

-- | Metadata with statically known fields
--
-- This is required for the response trailing metadata. When the server sends
-- the /initial/ set of headers to the client, it must tell the client which
-- trailers to expect (by means of the HTTP @Trailer@ header; see
-- <https://datatracker.ietf.org/doc/html/rfc7230#section-4.4>).
--
-- Any headers constructed in 'buildMetadata' /must/ be listed here; not doing
-- so is a bug. However, the converse is not true: it is acceptable for a header
-- to be listed in 'metadataHeaderNames' but not in 'buildMetadata'. Put another
-- way: the list of "trailers to expect" included in the initial request headers
-- is allowed to be an overapproximation, but not an underapproximation.
class BuildMetadata a => StaticMetadata a where
  metadataHeaderNames :: Proxy a -> [HeaderName]

-- | Parse metadata from custom metadata headers
--
-- Some guidelines for defining instances:
--
-- * You can assume that the list of headers will not contain duplicates. The
--   gRPC spec /does/ allow for duplicate headers and specifies how to process
--   them, but this will be taken care of before 'parseMetadata' is called.
-- * However, you should assume no particular /order/.
-- * If there are unexpected headers present, you have a choice whether you want
--   to consider this a error and throw an exception, or regard the additional
--   headers as merely additional information and simply ignore them. There is
--   no single right answer here: ignoring additional metadata runs the risk of
--   not realizing that the peer is trying to tell you something important, but
--   throwing an error runs the risk of unnecessarily aborting an RPC.
class ParseMetadata a where
  parseMetadata :: MonadThrow m => [CustomMetadata] -> m a

-- | Unexpected metadata
--
-- This exception can be thrown in 'ParseMetadata' instances. See 'ParseMetadata'
-- for discussion.
data UnexpectedMetadata = UnexpectedMetadata [CustomMetadata]
  deriving stock (Show)
  deriving anyclass (Exception)

