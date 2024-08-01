-- | RPC handlers
--
-- Intended for unqualified import.
module Network.GRPC.Server.Handler (
    RpcHandler(..)
  , hoistRpcHandler
    -- * Construction
  , mkRpcHandler
  , mkRpcHandlerNoInitialMetadata
    -- * Hide type argument
  , SomeRpcHandler(..)
  , someRpcHandler
  , hoistSomeRpcHandler
    -- * Execution
  , runHandler
  ) where

import Prelude hiding (lookup)

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Default
import Data.Kind
import Data.Proxy
import GHC.Stack
import Network.HTTP2.Internal qualified as HTTP2

import Network.GRPC.Server.Call
import Network.GRPC.Server.Context
import Network.GRPC.Spec
import Network.GRPC.Util.GHC
import Network.GRPC.Util.HTTP2.Stream (ClientDisconnected(..))
import Network.GRPC.Util.Session qualified as Session
import Debug.Trace (traceM)

{-------------------------------------------------------------------------------
  Handlers

  This is essentially an untyped interface; for a typed layer, see
  "Network.GRPC.Server.Protobuf".
-------------------------------------------------------------------------------}

-- | Handler for an RPC request
--
-- To construct an 'RpcHandler', you have two options:
--
-- * Use the \"raw\" API by calling 'mkRpcHandler'; this gives you full control
--   over the interaction with the client.
-- * Use the API from "Network.GRPC.Server.StreamType" to define handlers that
--   use the Protobuf stream types. This API is more convenient, and can be used
--   to guarantee at compile-time  that you have a handler for every method of
--   the services you support, but provides less flexibility (although it offers
--   an \"escape\" to the full API through
--   'Network.GRPC.Server.StreamType.RawMethod').
--
-- __Note on cancellation.__ The GRPC spec allows clients to \"cancel\" a
-- request (<https://grpc.io/docs/guides/cancellation/>). This does not
-- correspond to any specific message being sent across the network; instead,
-- the client simply disappears. The spec is quite clear that it is the
-- responsibility of the handler /itself/ to monitor for this. In @grapesy@ this
-- works as follows:
--
-- * Handlers are /not/ terminated when a client disappears. This allows the
--   handler to finish what it's doing, and terminate cleanly.
-- * When a handler tries to receive a message from the client ('recvInput'), or
--   send a message to the client ('sendOutput'), and the client disappeared,
--   this will result in a 'Network.GRPC.Server.ClientDisconnected' exception,
--   which the handler can catch and deal with.
--
-- Cancellation is always at the request of the /client/. If the /handler/
-- terminates early (that is, before sending the final output and trailers), a
-- 'Network.GRPC.Server.HandlerTerminated' exception will be raised and sent to
-- the client as 'GrpcException' with 'GrpcUnknown' error code.
data RpcHandler (m :: Type -> Type) (rpc :: k) = RpcHandler {
      -- | Handler proper
      runRpcHandler :: Call rpc -> m ()
    }

-- | Hoist an 'RpcHandler' to a different monad
--
-- We do not make 'RpcHandler' an instance of @MFunctor@ (from the @mmorph@
-- package) because @RpcHandler m@ is not a monad; this means that even though
-- the types line up, the concepts do not.
hoistRpcHandler ::
     (forall a. m a -> n a)
  -> RpcHandler m rpc
  -> RpcHandler n rpc
hoistRpcHandler f (RpcHandler h) = RpcHandler (f . h)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Constructor for 'RpcHandler'
--
-- When the handler sends its first message to the client, @grapesy@ must first
-- send the initial metadata (of type 'ResponseInitialMetadata') to the client.
-- This metadata can be updated at any point before that first message (for
-- example, after receiving some messages from the client) by calling
-- 'setResponseInitialMetadata'. If this function is never called, however, then
-- we need a default value; 'mkRpcHandler' therefore calls
-- 'setResponseInitialMetadata' once before the handler proper, relying on the
-- 'Default' instance.
--
-- For RPCs where a sensible default does not exist (perhaps the initial
-- response metadata needs the request metadata from the client, or even some
-- messages from the client), you can use 'mkRpcHandlerNoInitialMetadata'.
mkRpcHandler ::
     ( Default (ResponseInitialMetadata rpc)
     , MonadIO m
     )
  => (Call rpc -> m ()) -> RpcHandler m rpc
mkRpcHandler k = RpcHandler $ \call -> do
    liftIO $ setResponseInitialMetadata call def
    k call

-- | Variant on 'mkRpcHandler' that does not call 'setResponseInitialMetadata'
--
-- You /must/ call 'setResponseInitialMetadata' before sending the first
-- message. See 'mkRpcHandler' for additional discussion.
mkRpcHandlerNoInitialMetadata :: (Call rpc -> m ()) -> RpcHandler m rpc
mkRpcHandlerNoInitialMetadata = RpcHandler

{-------------------------------------------------------------------------------
  Hide the type argument
-------------------------------------------------------------------------------}

-- | Wrapper around 'RpcHandler' that hides the type argument
--
-- Construct using 'someRpcHandler'.
data SomeRpcHandler m = forall rpc.
     SupportsServerRpc rpc
  => SomeRpcHandler (Proxy rpc) (RpcHandler m rpc)

-- | Constructor for 'SomeRpcHandler'
someRpcHandler :: forall rpc m.
     SupportsServerRpc rpc
  => RpcHandler m rpc -> SomeRpcHandler m
someRpcHandler = SomeRpcHandler Proxy

hoistSomeRpcHandler ::
     (forall a. m a -> n a)
  -> SomeRpcHandler m
  -> SomeRpcHandler n
hoistSomeRpcHandler f (SomeRpcHandler p h) =
    SomeRpcHandler p (hoistRpcHandler f h)

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

-- | Accept incoming call
--
-- If the handler throws an exception, we will /attempt/ to inform the client
-- of what happened (see 'forwardException') before re-throwing the exception.
runHandler :: forall rpc.
     HasCallStack
  => (forall x. IO x -> IO x)
  -> Call rpc
  -> RpcHandler IO rpc
  -> IO ()
runHandler unmask call (RpcHandler k) = do
    -- http2 will kill the handler when the client disappears, but we want the
    -- handler to be able to terminate cleanly. We therefore run the handler in
    -- a separate thread, and wait for that thread to terminate.
    handlerThread <- asyncLabelled "grapesy:handler" handler
    waitForHandler unmask call handlerThread
  where
    -- The handler itself will run in a separate thread
    handler :: IO ()
    handler = do
        result <- try $ k call
        handlerTeardown result

    -- Deal with any exceptions thrown in the handler
    handlerTeardown :: Either SomeException () -> IO ()
    handlerTeardown (Right ()) = do
        -- Handler terminated successfully, but may not have sent final message.
        -- /If/ the final message was sent, 'forwardException' does nothing.
        forwarded <- forwardException call $ toException HandlerTerminated
        ignoreUncleanClose call $ ExitCaseSuccess ()
        when forwarded $
          -- The handler terminated before it sent the final message.
          throwM HandlerTerminated
    handlerTeardown (Left err) = do
        -- The handler threw an exception. Attempt to tell the client.
        void $ forwardException call err
        ignoreUncleanClose call $ ExitCaseException err
        throwM err

-- | Close the connection to the client, ignoring errors
--
-- An unclean shutdown can have 2 causes:
--
-- 1. We lost communication during the call.
--
--    We have no way of telling the client that something went wrong.
--
-- 2. The handler failed to properly terminate the communication
--    (send the final message and call 'waitForOutbound').
--
--    This is a bug in the handler, and is trickier to deal with. We don't
--    really know what state the handler left the channel in; for example,
--    we might have killed the thread halfway through sending a message.
--
--    So there not really anything we can do here (except perhaps show
--    the exception in 'serverTopLevel').
ignoreUncleanClose :: Call rpc -> ExitCase a -> IO ()
ignoreUncleanClose Call{callChannel} reason =
    void $ Session.close callChannel reason

-- | Wait for the handler to terminate
--
-- If we are interrupted while we wait, it depends on the interruption:
--
-- * If we are interrupted by 'HTTP2.KilledByHttp2ThreadManager', it means we
--   got disconnected from the client. In this case, we shut down the channel
--   (if it's not already shut down); /if/ the handler at this tries to
--   communicate with the client, an exception will be raised. However, the
--   handler /can/ terminate cleanly and, of course, typically will.
--   Importantly, this avoids race conditions: even if the server and the client
--   agree that no further communication takes place (the client sent their
--   final message, the server sent the trailers), without the indireciton of
--   this additional thread it can still happen that http2 kills the handler
--   (after the client disconnects) before the handler has a chance to
--   terminate.
--
-- * If we are interrupted by another kind of asynchronous exception, we /do/
--   kill the handler (this might for example be a timeout).
--
-- This is in line with the overall design philosophy of communication in this
-- library: exceptions will only be raised synchronously when communication is
-- attempted, not asynchronously when we notice a problem.
waitForHandler ::
     HasCallStack
  => (forall x. IO x -> IO x)
  -> Call rpc -> Async () -> IO ()
waitForHandler unmask call handlerThread = loop
  where
    loop :: IO ()
    loop = unmask (wait handlerThread) `catch` handleException

    handleException :: SomeException -> IO ()
    handleException err
      | Just (HTTP2.KilledByHttp2ThreadManager mErr) <- fromException err = do
          traceM $ "handleException: " ++ show mErr
          let exitReason :: ExitCase ()
              exitReason =
                case mErr of
                  Nothing -> ExitCaseSuccess ()
                  Just exitWithException ->
                    ExitCaseException . toException $
                      ClientDisconnected exitWithException callStack
          ignoreUncleanClose call exitReason
          loop

      | otherwise = do
          -- If we get an exception while waiting on the handler, there
          -- are two possibilities:
          --
          -- 1. The exception was an asynchronous exception, thrown to us
          --    externally. In this case @cancalWith@ will throw the
          --    exception to the handler (and wait for it to terminate).
          -- 2. The exception was thrown by the handler itself. In this
          --    case @cancelWith@ is a no-op.
          cancelWith handlerThread err
          throwM err

-- | Process exception thrown by a handler
--
-- Trace the exception and forward it to the client.
--
-- The attempt to forward it to the client is a best-effort only:
--
-- * The nature of the exception might mean that we we cannot send anything to
--   the client at all.
-- * It is possible the exception was thrown /after/ the handler already send
--   the trailers to the client.
--
-- We therefore catch and suppress all exceptions here. Returns @True@ if the
-- forwarding was successful, @False@ if it raised an exception.
forwardException :: Call rpc -> SomeException -> IO Bool
forwardException call@Call{callContext} err = do
    trailers <- serverExceptionToClientError (serverParams callContext) err
    (True <$ sendProperTrailers call trailers) `catch` handler
 where
   handler :: SomeException -> IO Bool
   handler _ = return False
