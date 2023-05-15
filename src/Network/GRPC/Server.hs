{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Server (
    -- * Handlers
    MethodHandler(..)
  , MethodHandlers(..)
  , MethodHandlersFor
  , ServiceHandlers(..)
    -- * Server proper
  , server
  ) where

import Data.HashMap.Strict
import Data.HashMap.Strict qualified as HashMap
import Data.Kind
import Data.ProtoLens.Service.Types
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeLits
import Network.GRPC.Spec
import Network.GRPC.Spec.HTTP2.Response qualified as Response
import Network.HTTP.Types
import Network.HTTP2.Server qualified as HTTP2

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

-- TODO: We again need some kind of Protobuf instantation of this, where the
-- type of the handler depends on the kind of RPC call it is.
newtype MethodHandler (s :: Type) (m :: Symbol) = MethodHandler {
      runMethodHandler :: IO ()
    }

data MethodHandlers (s :: Type) (ms :: [Symbol]) where
  AllMethodsHandled :: MethodHandlers s '[]
  HandleMethod ::
       HasMethod s m
    => MethodHandler s m
    -> MethodHandlers s ms
    -> MethodHandlers s (m ': ms)

type MethodHandlersFor s = MethodHandlers s (ServiceMethods s)

data ServiceHandlers (ss :: [Type]) where
  AllServicesHandled :: ServiceHandlers '[]
  HandleService ::
       Service s
    => MethodHandlersFor s
    -> ServiceHandlers ss
    -> ServiceHandlers (s ': ss)

{-------------------------------------------------------------------------------
  Server proper
-------------------------------------------------------------------------------}

server :: ServiceHandlers ss -> HTTP2.Server
server handlers req _aux respond = do
    print req
    respond (HTTP2.responseNoBody ok200 (Response.buildTrailers trailers)) []
  where
    trailers :: Trailers
    trailers = Trailers {
          trailerGrpcStatus  = GrpcError GrpcUnimplemented
        , trailerGrpcMessage = Just "This method is not implemented"
        , trailerCustom      = []
        }

    _rawHandlers :: RawServiceHandlers
    _rawHandlers = rawServiceHandlers handlers

{-------------------------------------------------------------------------------
  Untyped handlers
-------------------------------------------------------------------------------}

type RawServiceHandlers = HashMap Text RawMethodHandlers

type RawMethodHandlers = HashMap Text RawMethodHandler

type RawMethodHandler = ()

rawServiceHandlers :: ServiceHandlers ss -> RawServiceHandlers
rawServiceHandlers = go HashMap.empty
  where
    go :: RawServiceHandlers -> ServiceHandlers ss -> RawServiceHandlers
    go acc AllServicesHandled   = acc
    go acc (HandleService s ss) =
        go (HashMap.insert (name s) (rawMethodHandlers s) acc) ss

    name :: forall s ms. Service s => MethodHandlers s ms -> Text
    name _ = Text.pack $ symbolVal (Proxy @(ServiceName s))

rawMethodHandlers :: forall s ms. MethodHandlers s ms -> RawMethodHandlers
rawMethodHandlers = go HashMap.empty
  where
    go :: RawMethodHandlers -> MethodHandlers s ms' -> RawMethodHandlers
    go acc AllMethodsHandled   = acc
    go acc (HandleMethod m ms) =
        go (HashMap.insert (name m) (rawMethodHandler m) acc) ms

    name :: forall m. HasMethod s m => MethodHandler s m -> Text
    name _ = Text.pack $ symbolVal (Proxy @(MethodName s m))

rawMethodHandler :: MethodHandler s m -> RawMethodHandler
rawMethodHandler _ = ()