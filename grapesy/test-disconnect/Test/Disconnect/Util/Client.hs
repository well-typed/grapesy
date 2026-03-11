module Test.Disconnect.Util.Client (
    ClientThread(..)
  , forkClientThread
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

{-------------------------------------------------------------------------------
  General infrastructure
-------------------------------------------------------------------------------}

data ClientThread a = ClientThread{
      -- | Wait for the client to have established a connection to the server
      waitClientConnected :: IO (Either SomeException ())

      -- | Wait for the client to terminate and get its result
    , waitClientResult :: IO (Either SomeException a)

      -- | Kill the thread
    , killClientThread :: IO ()
    }

forkClientThread :: forall a.
     (    IO ()         -- ^ Mark client connected (no-op if called again)
       -> (a -> IO ())  -- ^ Mark client result
       -> IO ()
     )
  -> IO (ClientThread a)
forkClientThread k = do
    clientConnected <- newEmptyTMVarIO
    clientResult    <- newEmptyTMVarIO

    let recordException :: SomeException -> IO ()
        recordException e = atomically $ do
            -- It's possible (indeed, likely) the exception happened after
            -- we set up a connection
            void $ tryPutTMVar clientConnected $ Left e

            -- It should normally not happen that an exception is raised /after/
            -- the client is done. If it /does/ happen, we want to know about
            -- it, so we override the existing value, if any.
            void $ tryTakeTMVar clientResult
            putTMVar clientResult $ Left e

        markConnected :: IO ()
        markConnected = atomically $
            void $ tryPutTMVar clientConnected $ Right ()

        markResult :: a -> IO ()
        markResult result = atomically $
            putTMVar clientResult $ Right result

    clientThreadId <- forkIO $
      handle recordException $
        k markConnected markResult

    return $ ClientThread{
        waitClientConnected = atomically $ readTMVar clientConnected
      , waitClientResult    = atomically $ readTMVar clientResult
      , killClientThread    = killThread clientThreadId
      }
