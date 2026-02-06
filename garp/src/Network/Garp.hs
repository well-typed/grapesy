{-# LANGUAGE RankNTypes #-}
-- |  The code in this module is a cut down version of
-- <http://hackage.haskell.org/package/warp warp>, which is distributed under MIT license.
--
module Network.Garp (
    -- * Application
    Application,
    lazyIOApplication,
    -- * Running
    run,
    runSettings,
    Port,
    -- * Settings
    Settings,
    defaultSettings,
    -- ** Lenses
    settingsPort,
    settingsHost,
    settingsOnException,
    settingsOnOpen,
    settingsOnClose,
    settingsBeforeMainLoop,
    settingsFork,
    Fork (..),
    -- * Connection
    Connection (..),
    connLazyRead,
    connLazySendAll,
    ) where

import Control.Concurrent        (forkIOWithUnmask)
import Control.Exception         (SomeException (..), allowInterrupt, bracket, catch, displayException, finally, handle, mask_, toException, try)
import Control.Monad             (when)
import Data.ByteString           (ByteString)
import Data.Foldable             (traverse_)
import Data.Functor              (void)
import Foreign.C.Error           (Errno (..), eCONNABORTED)
import GHC.IO.Exception          (ioe_errno)
import Network.Run.TCP           (resolve, openTCPServerSocket)
import Network.Socket            (HostName, ServiceName, SockAddr, Socket, SocketOption (NoDelay), accept, close, setSocketOption, withSocketsDo, SocketType(Stream), AddrInfoFlag(AI_PASSIVE))
import Network.Socket.ByteString (recv, sendAll)
import System.IO                 (hPutStrLn, stderr)

-- Used to implement lazyIOApplication
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Internal as LBSI

-------------------------------------------------------------------------------
-- Application
-------------------------------------------------------------------------------

-- | An application is an action on a connection.
type Application = Connection -> IO ()

-- | Converts a lazy 'LBS.ByteString' endomorphism into an 'Application'.
lazyIOApplication :: (LBS.ByteString -> LBS.ByteString) -> Application
lazyIOApplication f conn = do
    lbs <- connLazyRead conn
    connLazySendAll conn (f lbs)

-------------------------------------------------------------------------------
-- Run
-------------------------------------------------------------------------------

-- | Run 'Application' with 'defaultSettings'.
run :: ServiceName -> Application -> IO ()
run p = runSettings defaultSettings { _settingsPort = p }

-- | Run 'Application' with 'Settings'.
runSettings :: Settings -> Application -> IO ()
runSettings set app = withSocketsDo $
    openTCPServer (_settingsHost set) (_settingsPort set) $ \socket ->
    runSettingsSocket set socket app

-------------------------------------------------------------------------------
-- Settings
-------------------------------------------------------------------------------

-- | TCP port number.
type Port = Int

-- | Various settings.
data Settings = Settings
    { _settingsPort           :: ServiceName
    , _settingsHost           :: Maybe HostName
    , _settingsOnException    :: SomeException -> IO ()
    , _settingsOnOpen         :: SockAddr -> IO Bool
    , _settingsOnClose        :: SockAddr -> IO ()
    , _settingsBeforeMainLoop :: IO ()
    , _settingsFork           :: ((forall a. IO a -> IO a) -> IO ()) -> IO ()
    }

-- |  The default settings for the server.
defaultSettings :: Settings
defaultSettings = Settings
    { _settingsPort           = "9999"
    , _settingsHost           = Just "0.0.0.0"
    , _settingsOnException    = hPutStrLn stderr . displayException
    , _settingsOnOpen         = \_ -> return True
    , _settingsOnClose        = \_ -> return ()
    , _settingsBeforeMainLoop = return ()
    , _settingsFork           = defaultSettingsFork
    }
  where
    defaultSettingsFork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ()
    defaultSettingsFork f = void (forkIOWithUnmask f)

-------------------------------------------------------------------------------
-- Settings lenses
-------------------------------------------------------------------------------

type Lens' s a = forall f. Functor f =>  (a -> f a) -> s -> f s

-- | Port to listen on. Default value: 9999
settingsPort :: Lens' Settings ServiceName
settingsPort f s = fmap (\x -> s { _settingsPort = x }) (f (_settingsPort s))
{-# INLINE settingsPort #-}

-- | Default value: 'HostIPv4'
settingsHost :: Lens' Settings (Maybe HostName)
settingsHost f s = fmap (\x -> s { _settingsHost = x }) (f (_settingsHost s))
{-# INLINE settingsHost #-}

-- | What to do with exceptions thrown by either the application or server. Default: ignore server-generated exceptions (see 'InvalidRequest') and print application-generated applications to stderr.
settingsOnException :: Lens' Settings (SomeException -> IO ())
settingsOnException f s = fmap (\x -> s { _settingsOnException = x }) (f (_settingsOnException s))
{-# INLINE settingsOnException #-}

-- | What to do when a connection is open.
--
-- When 'False' is returned, the connection is closed immediately.
-- Otherwise, the connection is going on.
--
-- Default: always returns 'True'.
settingsOnOpen :: Lens' Settings (SockAddr -> IO Bool)
settingsOnOpen f s = fmap (\x -> s { _settingsOnOpen = x }) (f (_settingsOnOpen s))
{-# INLINE settingsOnOpen #-}

-- | What to do when a connection is close. Default: do nothing.
settingsOnClose :: Lens' Settings (SockAddr -> IO ())
settingsOnClose f s = fmap (\x -> s { _settingsOnClose = x }) (f (_settingsOnClose s))
{-# INLINE settingsOnClose #-}

-- | Code to run after the listening socket is ready but before entering
-- the main event loop. Useful for signaling to tests that they can start
-- running, or to drop permissions after binding to a restricted port.
--
-- Default: do nothing.
settingsBeforeMainLoop :: Lens' Settings (IO ())
settingsBeforeMainLoop f s = fmap (\x -> s { _settingsBeforeMainLoop = x }) (f (_settingsBeforeMainLoop s))
{-# INLINE settingsBeforeMainLoop #-}

-- | Code to fork a new thread to accept a connection.
--
-- This may be useful if you need OS bound threads, or if
-- you wish to develop an alternative threading model.
--
-- Default: @void . forkIOWithUnmask@
settingsFork :: Lens' Settings Fork
settingsFork f s = fmap (\x -> s { _settingsFork = runFork x }) (f (Fork (_settingsFork s)))

-- | A newtype wrapper around polymorphic @fork@ type to avoid @ImpredicativeTypes@.
newtype Fork = Fork { runFork :: ((forall a. IO a -> IO a) -> IO ()) -> IO () }

-------------------------------------------------------------------------------
-- Connection
-------------------------------------------------------------------------------

-- | A very thin abstraction over a socket.
data Connection = Connection
    { connSendAll     :: ByteString -> IO ()
    -- ^ The sending function.
    , connClose       :: IO ()
    -- ^ The connection closing function. Warp guarantees it will only be
    -- called once. Other functions (like 'connRecv') may be called after
    -- 'connClose' is called.
    , connRecv        :: IO ByteString
    -- ^ The connection receiving function. This returns "" for EOF.
    }

-- | Lazily read the whole input from 'Connection' into lazy 'LBS.ByteString'.
connLazyRead :: Connection -> IO LBS.ByteString
connLazyRead conn = lazyRead
  where
    lazyRead = unsafeInterleaveIO loop
    loop = do
        c <- connRecv conn -- only blocks if there is no data available
        if BS.null c
        then connClose conn >> return LBSI.Empty
        else do
            cs <- lazyRead
            return (LBSI.Chunk c cs)

-- | Send all lazy 'LBS.ByteString' chunks one by one.
connLazySendAll :: Connection -> LBS.ByteString -> IO ()
connLazySendAll conn lbs = traverse_ (connSendAll conn) (LBS.toChunks lbs)

socketConnection :: Socket -> Connection
socketConnection s = Connection
    { connRecv    = recv s 4096
    , connSendAll = sendAll s
    , connClose   = close s
    }

-------------------------------------------------------------------------------
-- Low-level
-------------------------------------------------------------------------------

-- | Open server listening socket.
openTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO ()) -> IO ()
openTCPServer mhost port server = do
    addr <- resolve Stream mhost port [AI_PASSIVE] NE.head
    bracket (openTCPServerSocket addr) close server

runSettingsSocket :: Settings -> Socket -> Application -> IO ()
runSettingsSocket set socket app = do
    runSettingsConnection set getConn app
  where
    getConn = do
        (s, sa) <- accept socket
        -- NoDelay causes an error for AF_UNIX.
        setSocketOption socket NoDelay 1 `catch` \(SomeException _) -> return ()
        return (socketConnection s, sa)

    closeListenSocket = close socket

runSettingsConnection :: Settings -> IO (Connection, SockAddr) -> Application -> IO ()
runSettingsConnection set getConn app = do
    _settingsBeforeMainLoop set
    void $ mask_ acceptLoop
  where
    acceptLoop = do
        -- Allow async exceptions before receiving the next connection maker.
        allowInterrupt

        -- In contrast to warp, we create 'Connection' in the loop
        -- as our 'Connection's are cheap to create.
        mx <- acceptNewConnection
        case mx of
            Nothing          -> return ()
            Just (conn, addr) -> do
                fork set conn addr app
                acceptLoop

    acceptNewConnection = do
        ex <- try getConn
        case ex of
            Right x -> return (Just x)
            Left e -> do
                let eConnAborted = getErrno eCONNABORTED
                    getErrno (Errno cInt) = cInt
                if ioe_errno e == Just eConnAborted
                    then acceptNewConnection
                    else do
                        _settingsOnException set $ toException e
                        return Nothing

fork :: Settings -> Connection -> SockAddr -> Application -> IO ()
fork set conn addr app = _settingsFork set $ \unmask ->
    -- Call the user-supplied on exception code if any
    -- exceptions are thrown.
    handle (_settingsOnException set) $ finally (serve unmask) (connClose conn)
  where
    -- We need to register a timeout handler for this thread, and
    -- cancel that handler as soon as we exit. We additionally close
    -- the connection immediately in case the child thread catches the
    -- async exception or performs some long-running cleanup action.
    serve unmask = unmask $ do
       -- Call the user-supplied code for connection open and
       -- close events
       bracket onOpen onClose $ \goingon ->
           -- Actually serve this connection.  bracket with closeConn
           -- above ensures the connection is closed.
           when goingon $ serveConnection conn addr set app

    onOpen = _settingsOnOpen  set addr
    onClose _ = _settingsOnClose set addr

serveConnection :: Connection -> SockAddr -> Settings -> Application -> IO ()
serveConnection conn _addr _set app = app conn
