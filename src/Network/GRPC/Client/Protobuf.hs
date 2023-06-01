{-# LANGUAGE OverloadedStrings #-}

-- | gRPC with Protobuf
--
-- Intended for unqualified import.
module Network.GRPC.Client.Protobuf (
    RPC(..)
    -- * Handlers
  , NonStreamingHandler    -- opaque
  , ClientStreamingHandler -- opaque
  , ServerStreamingHandler -- opaque
  , BiDiStreamingHandler   -- opaque
    -- ** Execution
  , nonStreaming
  , clientStreaming
  , serverStreaming
  , biDiStreaming
    -- ** Construction
  , MakeHandler -- opaque
  , rpc
  , rpcWith
  ) where

import Control.Concurrent.Async
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.Default
import Data.ProtoLens.Service.Types
import Data.Proxy

import Network.GRPC.Client
import Network.GRPC.Common.Protobuf
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.Protobuf
import Network.GRPC.Util.RedundantConstraint

-- $specialized
--
-- The
-- [gRPC specification](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md)
-- does not distinguish between different kinds of communication patterns;
-- this is done in the
-- [Protobuf specification](https://protobuf.dev/reference/protobuf/proto3-spec/#service_definition).
-- We provide convenience functions here that capture these communication
-- patterns. It is important to realize however that these wrappers are simple;
-- if they do not provide the functionality you require, using the more general
-- interface from "Network.GRPC.Client" is not much more difficult.

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

-- | Make a non-streaming RPC
--
-- Example usage:
--
-- > nonStreaming @RouteGuide @"getFeature" (rpc conn) point
nonStreaming :: forall serv meth m.
     MethodStreamingType serv meth ~ NonStreaming
  => NonStreamingHandler m serv meth
  -> MethodInput serv meth
  -> m (MethodOutput serv meth)
nonStreaming (NonStreamingHandler h) = h
  where
    _ = addConstraint $ Proxy @(MethodStreamingType serv meth ~ NonStreaming)

-- | Make a client-side streaming RPC
--
-- Example usage:
--
-- > clientStreaming @RouteGuide @"recordRoute" (rpc conn) getPoint
clientStreaming :: forall serv meth m.
     MethodStreamingType serv meth ~ ClientStreaming
  => ClientStreamingHandler m serv meth
  -> m (StreamElem () (MethodInput serv meth))
  -> m (MethodOutput serv meth)
clientStreaming (ClientStreamingHandler h) = h
  where
    _ = addConstraint $ Proxy @(MethodStreamingType serv meth ~ ClientStreaming)

-- | Make a server-side streaming RPC
--
-- Example usage:
--
-- > serverStreaming @RouteGuide @"listFeatures" (rpc conn) rect usePoint
serverStreaming :: forall serv meth m.
     MethodStreamingType serv meth ~ ServerStreaming
  => ServerStreamingHandler m serv meth
  -> MethodInput serv meth
  -> (MethodOutput serv meth -> m ())
  -> m ()
serverStreaming (ServerStreamingHandler h) = h
  where
    _ = addConstraint $ Proxy @(MethodStreamingType serv meth ~ ServerStreaming)

-- | Make a bidirectional RPC
--
-- Example usage:
--
-- > biDiStreaming @RouteGuide @"routeChat" (rpc conn) getNote useNote
biDiStreaming :: forall serv meth m.
     MethodStreamingType serv meth ~ BiDiStreaming
  => BiDiStreamingHandler m serv meth
  -> m (StreamElem () (MethodInput serv meth))
  -> (MethodOutput serv meth -> m ())
  -> m ()
biDiStreaming (BiDiStreamingHandler h) = h
  where
    _ = addConstraint $ Proxy @(MethodStreamingType serv meth ~ BiDiStreaming)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

class MakeHandler h where
  mkHandler ::
       IsRPC (RPC serv meth)
    => Connection
    -> CallParams
    -> RPC serv meth
    -> h serv meth

-- | Construct RPC handler
--
-- See 'nonStreaming' and friends for example usage.
rpc :: forall serv meth h.
     ( MakeHandler h
     , IsRPC (RPC serv meth)
     )
  => Connection -> h serv meth
rpc conn = rpcWith conn def

-- | Generalization of 'rpc' with 'CallParams'
rpcWith :: forall serv meth h.
     ( MakeHandler h
     , IsRPC (RPC serv meth)
     )
  => Connection -> CallParams -> h serv meth
rpcWith conn params = mkHandler conn params (RPC @serv @meth)

instance ( MonadIO   m
         , MonadMask m
         ) => MakeHandler (NonStreamingHandler m) where
  mkHandler conn params proxy =
    NonStreamingHandler $ \input ->
      withRPC conn params proxy $ \call -> do
        sendOnlyInput call input
        (output, _trailers) <- recvOnlyOutput call
        return output

instance ( MonadIO   m
         , MonadMask m
         ) => MakeHandler (ClientStreamingHandler m) where
  mkHandler conn params proxy =
    ClientStreamingHandler $ \produceInput ->
      withRPC conn params proxy $ \call -> do
        sendAllInputs call produceInput
        (output, _trailers) <- recvOnlyOutput call
        return output

instance ( MonadIO   m
         , MonadMask m
         ) => MakeHandler (ServerStreamingHandler m) where
  mkHandler conn params proxy =
    ServerStreamingHandler $ \input processOutput ->
      withRPC conn params proxy $ \call -> do
        sendOnlyInput call input
        _trailers <- recvAllOutputs call processOutput
        return ()

-- | Bidirectional streaming
--
-- Unlike the other functions, we have not generalized this to an arbitrary
-- monad @m@, because it spawns a new thread.
--
-- TODO: This is an indication that we should reconsider the signature of this
-- function. Another indication that we should is that the corresponding
-- function in "Network.GRPC.Client.Protobuf.Pipes" cannot be defined in terms
-- of this function, unlike for the other streaming functions.
instance MakeHandler (BiDiStreamingHandler IO) where
  mkHandler conn params proxy =
    BiDiStreamingHandler $ \produceInput processOutput ->
      withRPC conn params proxy $ \call -> do
        withAsync (sendAllInputs call produceInput) $ \_sendAllThread -> do
          _trailers <- recvAllOutputs call processOutput
          return ()
