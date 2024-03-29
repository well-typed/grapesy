{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Spec.RPC.Unknown (UnknownRpc) where

import Data.Text qualified as Text
import Data.Typeable
import Data.Void
import GHC.Stack
import GHC.TypeLits

import Network.GRPC.Spec.RPC

-- | Unknown RPC
--
-- When a request comes in a gRPC server for an unknown method, the server is
-- supposed to respond with a @Trailers-Only@ message, with an @UNIMPLEMENTED@
-- error code. Frustratingly, @Trailers-Only@ requires a @Content-Type@ header,
-- /even though there is no content/. This @Content-Type@ header normally
-- indicates the serialization format (e.g., @application/grpc+proto@), but this
-- format depends on the specific method, which was not found!
--
-- To resolve this catch-22 the @Content-Type@ for 'UnknownRpc' is simply
-- @application/grpc@, with no format specifier. Fortunately, this is allowed
-- by the spec.
data UnknownRpc (serv :: Maybe Symbol) (meth :: Maybe Symbol)

instance ( MaybeKnown serv
         , MaybeKnown meth
         ) => IsRPC (UnknownRpc serv meth) where
  type Input  (UnknownRpc serv meth) = Void
  type Output (UnknownRpc serv meth) = Void

  rpcContentType         = const "application/grpc"
  rpcServiceName         = const $ Text.pack $ maybeSymbolVal (Proxy @serv)
  rpcMethodName          = const $ Text.pack $ maybeSymbolVal (Proxy @meth)
  rpcMessageType         = const "Void"
  rpcSerializeInput      = const absurd
  rpcSerializeOutput     = const absurd
  rpcDeserializeInput    = const $ \_ -> Left "absurd"
  rpcDeserializeOutput   = const $ \_ -> Left "absurd"

class Typeable msym => MaybeKnown (msym :: Maybe Symbol) where
  maybeSymbolVal :: HasCallStack => Proxy msym -> String

instance MaybeKnown Nothing where
  maybeSymbolVal = error "unknown"

instance KnownSymbol sym => MaybeKnown (Just sym) where
  maybeSymbolVal _ = symbolVal (Proxy @sym)

