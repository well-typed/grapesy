module Test.Disconnect.Util.Server (
    -- * Monitoring server handlers
    HandlerResults(..)
  , monitoredHandler
  , getHandlerResults
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException, fromException)
import Control.Exception qualified as Exception
import Control.Monad

import Network.GRPC.Common
import Network.GRPC.Server qualified as Server

{-------------------------------------------------------------------------------
  Monitoring server handlers
-------------------------------------------------------------------------------}

data HandlerResults = HandlerResults{
      -- | Number of normal terminations
      handlerNormalTerminations :: TVar Int

      -- | Number of terminations due to 'ClientDisconnected' exceptions
    , handlerClientDisconnected :: TVar Int

      -- | Terminations due to unexpected exceptions
    , handlerUnexpectedExceptions :: TVar [SomeException]
    }

-- | Construct 'TVar' that records the result of each handler invocation
monitoredHandler :: forall rpc.
     ( SupportsServerRpc rpc
     , Default (ResponseInitialMetadata rpc)
     )
  => (Server.Call rpc -> IO ())
  -> IO (Server.SomeRpcHandler IO, HandlerResults)
monitoredHandler handler = do
    handlerNormalTerminations   <- newTVarIO 0
    handlerClientDisconnected   <- newTVarIO 0
    handlerUnexpectedExceptions <- newTVarIO []

    let handlerFailed :: SomeException -> IO ()
        handlerFailed e = atomically $
            case fromException e of
              Just Server.ClientDisconnected{} ->
                modifyTVar handlerClientDisconnected (+ 1)
              _otherwise ->
                modifyTVar handlerUnexpectedExceptions (e :)

        handlerTerminated :: IO ()
        handlerTerminated = atomically $
            modifyTVar handlerNormalTerminations (+ 1)

    return (
        Server.someRpcHandler $ Server.mkRpcHandler @rpc $ \call ->
          Exception.handle handlerFailed $ do
            handler call
            handlerTerminated

      , HandlerResults{
            handlerNormalTerminations
          , handlerClientDisconnected
          , handlerUnexpectedExceptions
          }
      )

-- | Get handler results
getHandlerResults ::
     Int             -- ^ Timeout
  -> HandlerResults  -- ^ Monitored handler
  -> Int
     -- ^ Number of expected results
     --
     -- Blocks when not all results are available yet (/client/ termination does
     -- not guarantee that /server/ results are available, the handler might not
     -- yet have got a chance to say that it is done).
  -> IO (Int, Int, [SomeException])
getHandlerResults maxWait handlerResults expectedNumResults = do
    timeoutVar <- newTVarIO False
    void $ forkIO $ do
        threadDelay maxWait
        atomically $ writeTVar timeoutVar True
    atomically $ do
      normalTerminations   <- readTVar handlerNormalTerminations
      clientDisconnected   <- readTVar handlerClientDisconnected
      unexpectedExceptions <- readTVar handlerUnexpectedExceptions
      timeout              <- readTVar timeoutVar

      let results    = (
              normalTerminations
            , clientDisconnected
            , unexpectedExceptions
            )
          numResults = sum [
              normalTerminations
            , clientDisconnected
            , length unexpectedExceptions
            ]

      if numResults == expectedNumResults then
        return results
      else if timeout then
        throwSTM $ userError $ "getHandlerResults timeout: " ++ show results
      else
        retry
  where
    HandlerResults{
        handlerNormalTerminations
      , handlerClientDisconnected
      , handlerUnexpectedExceptions
      } = handlerResults
