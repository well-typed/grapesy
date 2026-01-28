module Network.GRPC.Common.ProtocolException (
    ProtocolException(..),
    SomeProtocolException(..),
) where

import Network.GRPC.Util.Imports

{-------------------------------------------------------------------------------
  Exceptions
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
  | TooFewOutputs (ResponseTrailingMetadata rpc)

    -- | We expected trailers, but got an output instead
  | TooManyOutputs (Output rpc)

    -- | The server unexpectedly used the Trailers-Only case
  | UnexpectedTrailersOnly (ResponseTrailingMetadata rpc)

deriving stock instance IsRPC rpc => Show (ProtocolException rpc)

-- | Existential wrapper around 'ProtocolException'
--
-- This makes it easier to catch these exceptions (without this, you'd have to
-- catch the exception for a /specific/ instance of @rpc@).
data SomeProtocolException where
    ProtocolException :: forall rpc.
         IsRPC rpc
      => ProtocolException rpc
      -> SomeProtocolException

deriving stock    instance Show SomeProtocolException
deriving anyclass instance Exception SomeProtocolException
