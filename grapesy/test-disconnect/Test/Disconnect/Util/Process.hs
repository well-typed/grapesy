module Test.Disconnect.Util.Process (
    -- * Process lifecycle
    ChildProcess(..)
  , forkChildProcess
  , killThisProcess
    -- * Simple IPC
  , IPC(..)
  , withIPC
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Proxy
import System.Exit
import System.IO
import System.IO.Temp
import System.Posix

{-------------------------------------------------------------------------------
  Process lifecycle
-------------------------------------------------------------------------------}

data ChildProcess = ChildProcess{
      waitForChildProcess :: IO (Maybe ProcessStatus)
    , killChildProcess    :: IO ()
    }

-- | Fork child process
forkChildProcess :: IO () -> IO ChildProcess
forkChildProcess k = do
    child <- forkProcess k
    return $ ChildProcess{
        waitForChildProcess = getProcessStatus True False child
      , killChildProcess    = signalProcess sigKILL child
      }

-- | Unclean process termination
--
-- We need to use this to properly simulate the execution environment crashing
-- in an unrecoverable way. In particular, we don't want to give the program a
-- chance to do any of its normal exception handling/cleanup behavior.
killThisProcess :: IO ()
killThisProcess = exitImmediately $ ExitFailure 1

{-------------------------------------------------------------------------------
  Simple IPC
-------------------------------------------------------------------------------}

data IPC a = IPC{
      ipcWrite :: a -> IO ()
    , ipcRead  :: IO a
    }

-- | Setup interprocess communicaton
--
-- We use a temporary file as a very rudimentary means of inter-process
-- communication so the server (which runs in a separate process) can make the
-- client aware of the port it is assigned by the OS.
withIPC :: forall a r. (Show a, Read a) => Proxy a -> (IPC a -> IO r) -> IO r
withIPC _ k =
    withTemporaryFile $ \ipcFile -> do
      let ipcWrite :: a -> IO ()
          ipcWrite x = writeFile ipcFile (show x)

          ipcRead :: IO a
          ipcRead = do
              threadDelay 10_000
              ma <- withFile ipcFile ReadWriteMode $ \h -> do
                sz <- hFileSize h
                if sz == 0 then
                  return Nothing
                else do
                  contents <- replicateM (fromIntegral sz) $ hGetChar h
                  value    <- evaluate $ read contents
                  -- Clear the contents, so we can send the next value
                  hSetFileSize h 0
                  return $ Just value

              -- If necessary, retry (outside the scope of 'withFile')
              maybe ipcRead return ma

      k IPC{ipcWrite, ipcRead}

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

withTemporaryFile :: (FilePath -> IO a) -> IO a
withTemporaryFile k =
    withSystemTempFile "grapesy-test-disconnect.txt" $ \fp h -> do
      hClose h
      k fp
