module Network.GRPC.Spec.Call (
    -- * Parameters
    CallParams -- opaque
  , callTimeout
  , callRequestMetadata
  ) where

import Data.Default
import Data.Functor.Const

import Network.GRPC.Spec.CustomMetadata.Typed
import Network.GRPC.Spec.Timeout

{-------------------------------------------------------------------------------
  Parameters

  NOTE: 'CallParams' is re-exported as is in the Client API.
-------------------------------------------------------------------------------}

-- | RPC parameters that can be chosen on a per-call basis
data CallParams rpc = CallParams {
      -- | Timeout
      --
      -- Tell the server that if the request cannot be completed within the
      -- specified amount of time, it should be aborted. If the timeout gets
      -- exceeded, a 'Network.GRPC.Common.GrpcDeadlineExceeded' exception will
      -- be raised.
      callTimeout :: Maybe Timeout

      -- | Custom metadata to be included in the request
    , callRequestMetadata :: RequestMetadata rpc

      -- | Type hack: fix the type of @rpc@
      --
      -- Without the presence of this field, something like
      --
      -- > callParams :: CallParams SomeSpecificRPC
      -- > callParams = def { callRequestMetata = .. }
      --
      -- would result in a type error about @rpc@ being ambiguous for @def@;
      -- after all, we have a type changing override here, so the choice of
      -- @rpc@ for @def@ is undetermined. The presence of 'callFixTypeOfRPC'
      -- (which is not exported) means that field overrides will never change
      -- the overall type of the parameters.
    , callFixTypeOfRPC :: Const () rpc
    }

deriving instance (Show (RequestMetadata rpc)) => Show (CallParams rpc)

-- | Default 'CallParams'
instance Default (RequestMetadata rpc) => Default (CallParams rpc) where
  def = CallParams {
        callTimeout         = Nothing
      , callRequestMetadata = def
      , callFixTypeOfRPC    = Const ()
      }

