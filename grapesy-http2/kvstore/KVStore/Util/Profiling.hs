{-# LANGUAGE CPP #-}

-- | Profiling utilities
--
-- Function 'markEvent' is normally just an alias for 'traceEventIO'. However,
-- when grapey is compiled with the @strace@ flag enabled, 'markEvent' also
-- writes the string to /dev/null using a posix @write@ call. This will show
-- up in the output of @strace@, thereby making it possible to relate strace
-- events to user events.
module KVStore.Util.Profiling (
    markEvent
  , markNonStreaming
  ) where

import Debug.Trace (traceEventIO)
import Control.Exception (bracket_)
import Network.GRPC.Common.StreamType
import Network.GRPC.Common
import Network.GRPC.Server.StreamType (ServerHandler')
import Network.GRPC.Server.StreamType qualified as Server

#ifdef STRACE
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)
import System.Posix
#endif

{-------------------------------------------------------------------------------
  markEvent
-------------------------------------------------------------------------------}

#ifdef STRACE

devNull :: Fd
{-# NOINLINE devNull #-}
devNull = unsafePerformIO $ do
#if MIN_VERSION_unix(2,8,0)
   openFd "/dev/null" WriteOnly defaultFileFlags
#else
   openFd "/dev/null" WriteOnly Nothing defaultFileFlags
#endif

markEvent :: String -> IO ()
markEvent str' = do
    -- We also write to /dev/null; the benefit of doing this is that this will
    -- show up in the strace output, allowing us to relate events to syscalls.
    _bytesWritten <- fdWrite devNull str
    when (fromIntegral _bytesWritten /= length str) $
      error "markEvent: fdWrite failed to write complete string"
    traceEventIO str
  where
    str = str' ++ "\n"

#else

markEvent :: String -> IO ()
markEvent = traceEventIO

#endif

{-------------------------------------------------------------------------------
  Derived
-------------------------------------------------------------------------------}

markNonStreaming ::
     SupportsStreamingType rpc NonStreaming
  => String
  -> (Input rpc -> IO (Output rpc))
  -> ServerHandler' NonStreaming IO rpc
markNonStreaming label handler = Server.mkNonStreaming $ \inp ->
    bracket_ (markEvent $ "HANDLER start " ++ label)
             (markEvent $ "HANDLER stop  " ++ label)
             (handler inp)