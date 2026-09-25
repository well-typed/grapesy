-- | Test servers that process individual HTTP2 frames
--
-- Main reference: <https://www.rfc-editor.org/rfc/rfc9113.html>
--
-- Instead for qualified import.
--
-- > import Test.Util.FrameLevelServer (Script, Frame(..), FrameHeader(..))
-- > import Test.Util.FrameLevelServer qualified as Frame
module Test.Util.FrameLevelServer (
    -- * Frames
    FrameHeader(..)
  , Frame(..)
  , mkFrame
    -- * Scripts
  , Script -- opaque
  , withScript
    -- ** Primitives
  , recv
  , ignore
  , send
    -- ** Standard building blocks
  , handshake
  , recvUntilEndStream
  , defaultNoiseFilter
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Async.Internal qualified as Async.Internal
import Control.Concurrent.STM
import Control.Exception
import Control.Exception.Annotation
import Control.Monad
import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Binary
import Data.Binary.Put qualified as Binary
import Data.Bits
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Either (partitionEithers)
import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Void
import Data.Word
import Network.GRPC.Client qualified as Client
import Network.Socket
import Network.Socket.ByteString qualified as Socket
import System.IO (fixIO)

{-------------------------------------------------------------------------------
  Scripts
-------------------------------------------------------------------------------}

data Script :: Type -> Type where
  Recv   :: (Frame -> Either String a) -> (a -> Script b) -> Script b
  Ignore :: (Frame -> Bool) -> Script a -> Script a
  Send   :: Frame -> Script a -> Script a
  Done   :: a -> Script a

-- | Receive frame of specified shape, or fail on unexpected frames
recv :: (Frame -> Either String a) -> Script a
recv p = Recv p Done

-- | Install new noise filter
ignore :: (Frame -> Bool) -> Script ()
ignore p = Ignore p $ Done ()

-- | Send frame
send :: Frame -> Script ()
send f = Send f $ Done ()

instance Functor Script where
  fmap = liftM

instance Applicative Script where
  pure  = Done
  (<*>) = ap

instance Monad Script where
  Recv   p k >>= l = Recv   p (k >=> l)
  Ignore p k >>= l = Ignore p (k >>= l)
  Send   f k >>= l = Send   f (k >>= l)
  Done x     >>= l = l x

withScript :: forall a r.
     Maybe ServiceName
  -> Script a
  -> (Client.Server -> GetHandlerResults a -> IO r)
  -> IO r
withScript service script k =
    withServer service handler $ \_host port getHandlerResults -> do
      let server :: Client.Server
          server = Client.ServerInsecure $ Client.Address{
              addressHost      = "127.0.0.1"
            , addressPort      = port
            , addressAuthority = Nothing
            }
      k server getHandlerResults
  where
    handler :: ServerHandler a
    handler clientSock clientAddr = do
        consumePreface clientSock
        runScript clientSock clientAddr script

    consumePreface :: Socket -> IO ()
    consumePreface clientSock = do
        mPreface <- fmap BS.Lazy.unpack <$> recvExact clientSock 24
        unless (mPreface == Just preface) $
          fail $ "Expected preface, got " ++ show mPreface

    preface :: [Word8]
    preface = [
          0x50, 0x52, 0x49, 0x20, 0x2a, 0x20
        , 0x48, 0x54, 0x54, 0x50, 0x2f, 0x32
        , 0x2e, 0x30, 0x0d, 0x0a, 0x0d, 0x0a
        , 0x53, 0x4d, 0x0d, 0x0a, 0x0d, 0x0a
        ]

runScript :: Socket -> SockAddr -> Script a -> IO a
runScript clientSock _clientAddr = go (const False)
  where
    go :: (Frame -> Bool) -> Script a -> IO a
    go noiseFilter = \case
        Recv p k -> do
          mFrame <- recvFrame clientSock
          case mFrame of
            Nothing ->
              fail "Unexpected EOF"
            Just frame | noiseFilter frame ->
              go noiseFilter (Recv p k)
            Just frame ->
              case p frame of
                Left err -> fail err
                Right a  -> go noiseFilter $ k a
        Ignore p k -> do
          go p k
        Send f k -> do
          sendFrame clientSock f
          go noiseFilter k
        Done a -> do
          skipRemainder noiseFilter
          return a

    skipRemainder :: (Frame -> Bool) -> IO ()
    skipRemainder noiseFilter = do
        mFrame <- recvFrame clientSock
        case mFrame of
          Nothing ->
            return ()
          Just frame | noiseFilter frame ->
            skipRemainder noiseFilter
          Just frame ->
            fail $ "Expected EOF, but got " ++ show frame

{-------------------------------------------------------------------------------
  Script building blocks
-------------------------------------------------------------------------------}

-- | Frames that may arrive at any point, and that scripts don't care about
--
-- * SETTINGS with ACK: the client acknowledging ours (see 'handshake')
-- * WINDOW_UPDATE: flow-control credit; needs no reply
-- * GOAWAY with NO_ERROR: the client's clean shutdown
--
-- Deliberately excluded: SETTINGS without ACK ('handshake' waits for it),
-- GOAWAY with an error code (that is information, not noise), and PING
-- (a PING without ACK requires a reply, RFC 9113 section 6.7, which a
-- filter cannot give).
defaultNoiseFilter :: Frame -> Bool
defaultNoiseFilter frame =
    case frameType of
      0x4 -> frameFlags .&. 0x1 /= 0   -- SETTINGS ACK
      0x8 -> True                      -- WINDOW_UPDATE
      0x7 -> errorCode == noError      -- GOAWAY
      _   -> False
  where
    Frame{
        frameHeader = FrameHeader{frameType, frameFlags}
      , framePayload
      } = frame

    -- GOAWAY payload: last stream ID (4 octets), error code (4 octets), debug
    errorCode = BS.Lazy.take 4 (BS.Lazy.drop 4 framePayload)
    noError   = BS.Lazy.replicate 4 0

-- | Connection preface exchange (RFC 9113 section 3.4)
--
-- The client's SETTINGS is guaranteed to be its first frame, so this can be
-- straight-line. Sending our SETTINGS means the client's ACK of it arrives at
-- some unpredictable later point, so we install the default noise filter from
-- here on; scripts needing more can call 'ignore' afterwards.
handshake :: Script ()
handshake = do
    send $ Frame (FrameHeader 0 0x4 0x0 0) BS.Lazy.empty   -- our SETTINGS
    recv $ \frame ->
      case frameHeader frame of
        FrameHeader{frameType = 0x4, frameFlags, frameStreamId = 0}
          | frameFlags .&. 0x1 == 0 -> Right ()
        _otherwise -> Left $ "Expected client SETTINGS, got " ++ show frame
    send $ Frame (FrameHeader 0 0x4 0x1 0) BS.Lazy.empty   -- ACK theirs
    ignore defaultNoiseFilter

-- | Receive DATA frames on the given stream, up to and including END_STREAM
--
-- Returns the concatenated payloads.
recvUntilEndStream :: Word32 -> Script Lazy.ByteString
recvUntilEndStream sid = go []
  where
    go :: [Lazy.ByteString] -> Script Lazy.ByteString
    go acc = do
        (payload, endStream) <- recv $ \frame ->
          case frameHeader frame of
            FrameHeader{frameType = 0x0, frameFlags, frameStreamId}
              | frameStreamId == sid ->
                  Right (framePayload frame, frameFlags .&. 0x1 /= 0)
            _otherwise ->
              Left $ "Expected DATA on stream " ++ show sid ++ ", got " ++ show frame
        let acc' = payload : acc
        if endStream
          then return $ BS.Lazy.concat (reverse acc')
          else go acc'

{-------------------------------------------------------------------------------
  HTTP2 frames

  > HTTP Frame {
  >   Length (24),
  >   Type (8),
  >
  >   Flags (8),
  >
  >   Reserved (1),
  >   Stream Identifier (31),
  >
  >   Frame Payload (..),
  > }
-------------------------------------------------------------------------------}

data FrameHeader = FrameHeader{
      frameLength   :: Word16  -- really 24 bits
    , frameType     :: Word8
    , frameFlags    :: Word8
    , frameStreamId :: Word32  -- really 31 bits
    }
  deriving stock (Show)

-- | HTTP2 frame
--
-- Invariant: frameLength frameHeader == length framePayload
data Frame = Frame{
      frameHeader  :: FrameHeader
    , framePayload :: Lazy.ByteString
    }
  deriving stock (Show)

mkFrame ::
    Word8            -- ^ Frame type
 -> Word8            -- ^ Flags
 -> Word32           -- ^ Stream ID
 -> Lazy.ByteString  -- ^ Payload
 -> Frame
mkFrame frameType frameFlags frameStreamId framePayload
  | BS.Lazy.length framePayload >= 65536
  = error "too large"

  | otherwise
  = Frame{
      frameHeader = FrameHeader{
          frameLength = fromIntegral $ BS.Lazy.length framePayload
        , frameType
        , frameFlags
        , frameStreamId
        }
    , framePayload
    }

instance Binary FrameHeader where
  get = do
      -- The spec already treats 2^14 the default upper limit, so we just
      -- restrict ourselves to 16-bit sizes here
      sizeMSB       <- Binary.getWord8
      frameLength   <- Binary.getWord16be
      frameType     <- Binary.getWord8
      frameFlags    <- Binary.getWord8
      frameStreamId <- (.&. 0x7FFFFFFF) <$> Binary.getWord32be
      unless (sizeMSB == 0) $ fail "too large"
      return FrameHeader{
          frameLength
        , frameType
        , frameFlags
        , frameStreamId
        }

  put header = mconcat [
        Binary.putWord8    0
      , Binary.putWord16be frameLength
      , Binary.putWord8    frameType
      , Binary.putWord8    frameFlags
      , Binary.putWord32be frameStreamId
      ]
    where
      FrameHeader{
          frameLength
        , frameType
        , frameFlags
        , frameStreamId
        } = header

recvFrame :: Socket -> IO (Maybe Frame)
recvFrame sock = do
    mHeader <- recvBinary sock 9
    case mHeader of
      Nothing     -> return Nothing
      Just header -> do
        mPayload <- recvExact sock (fromIntegral $ frameLength header)
        case mPayload of
          Nothing      -> fail "Missing payload"
          Just payload -> return $ Just Frame{
              frameHeader  = header
            , framePayload = payload
            }

sendFrame :: Socket -> Frame -> IO ()
sendFrame sock Frame{frameHeader, framePayload} = do
    sendBinary sock frameHeader
    Socket.sendMany sock $ BS.Lazy.toChunks framePayload

{-------------------------------------------------------------------------------
  Server
-------------------------------------------------------------------------------}

-- | Get the results of all handlers
--
-- Should only be called once all clients have disconnected.
type GetHandlerResults a = IO [a]

withServer :: forall a r.
     Maybe ServiceName
  -> ServerHandler a
  -> (HostAddress -> PortNumber -> GetHandlerResults a -> IO r)
  -> IO r
withServer service handler k = do
    serverState <- initServerState
    let server :: IO Void
        server = runServer (Just "127.0.0.1") service serverState handler

    withAsync server $ \serverThread -> do
      link serverThread
      (addr, port) <- readMVar (serverAddress serverState)
      let getHandlerResults :: IO [a]
          getHandlerResults = do
            handlers <- readMVar (serverHandlers serverState)
            mapM wait handlers
      k addr port getHandlerResults `catchNoPropagate` \(e :: (ExceptionWithContext SomeException)) -> do
        -- Check for failed handlers, but avoid waiting
        handlers <- readMVar (serverHandlers serverState)
        results  <- mapM poll handlers
        let failed = fst $ partitionEithers $ catMaybes results
        annotateIO (FailedHandlers failed) $ rethrowIO e

data FailedHandlers = FailedHandlers [SomeException]
  deriving stock (Show)
  deriving anyclass (ExceptionAnnotation)

type ServerHandler a = Socket -> SockAddr -> IO a

data ServerState a = ServerState{
      -- | Server address, once it's running
      serverAddress  :: MVar (HostAddress, PortNumber)

      -- | All server handlers ever spawned
      --
      -- This is an obvious memory leak, but that's irrelevant for a testing
      -- server: this allows to inspect the result of each handler in the test.
    , serverHandlers :: MVar [Async a]
    }

initServerState :: IO (ServerState a)
initServerState =
    pure ServerState
      <*> newEmptyMVar
      <*> newMVar []

runServer :: forall a.
     Maybe HostName
  -> Maybe ServiceName
  -> ServerState a
  -> ServerHandler a
  -> IO Void
runServer host service serverState handler = do
    addrInfo <- NE.head <$> getAddrInfo (Just hints) host service
    bracket (openSocket addrInfo) close $ \serverSock -> do
      setSocketOption serverSock ReuseAddr 1
      bind serverSock $ addrAddress addrInfo
      listen serverSock maxListenQueue

      serverAddr <- getSocketName serverSock
      case serverAddr of
        SockAddrInet port addr -> putMVar (serverAddress serverState) (addr, port)
        SockAddrInet6{} -> error "unexpected IPv6 socket"
        SockAddrUnix{}  -> error "unexpected unix socket"

      forever $ mask_ $ do
        (clientSock, clientAddr) <- accept serverSock -- interruptible call
        let handler' :: (forall x. IO x -> IO x) -> IO a
            handler' unmask = unmask $ do
              a <- handler clientSock clientAddr
              gracefulClose clientSock gracefulTimeout
              return a
        -- We use 'asyncFinally' to ensure that if an exception is thrown in the
        -- handler, it is recorded /before/ the socket is closed, so that if
        -- that socket closure results in an exception in the test (client)
        -- code, we are sure that the handler exception /has/ been recorded.
        asyncFinally
          (serverHandlers serverState)
          handler'
          (\_ -> close clientSock)
  where
    hints :: AddrInfo
    hints = defaultHints{
          addrFlags      = [AI_PASSIVE] -- socket suitable for 'accept'
        , addrFamily     = AF_INET      -- IPv4 only
        , addrSocketType = Stream       -- TCP, not UDP
        }

    gracefulTimeout :: Int
    gracefulTimeout = 5000 -- ms

{-------------------------------------------------------------------------------
  Internal auxiliary: network
-------------------------------------------------------------------------------}

recvExact :: Socket -> Int -> IO (Maybe Lazy.ByteString)
recvExact sock = \n -> go n []
  where
    go :: Int -> [Strict.ByteString] -> IO (Maybe Lazy.ByteString)
    go 0 acc = return $ Just $ BS.Lazy.fromChunks (reverse acc)
    go n acc = do
        chunk <- Socket.recv sock (min n 4096)
        if BS.Strict.null chunk then
          case acc of
            [] -> return Nothing
            _  -> fail "Peer closed connection"
        else
          go (n - BS.Strict.length chunk) (chunk : acc)

recvBinary :: Binary a => Socket -> Int -> IO (Maybe a)
recvBinary sock sz = do
    mBytes <- recvExact sock sz
    case mBytes of
      Nothing    -> return Nothing
      Just bytes -> do
        case Binary.decodeOrFail bytes of
          Left (_, _, err) -> fail err
          Right (unconsumed, sz', a) -> do
            unless (BS.Lazy.null unconsumed) $
              fail $ "Unexpected unconsumed bytes " ++ show unconsumed
            unless (fromIntegral sz == sz') $
              fail $ "Unexpected size " ++ show sz' ++ ". Expected " ++ show sz
            return (Just a)

sendBinary :: Binary a => Socket -> a -> IO ()
sendBinary sock = Socket.sendMany sock . BS.Lazy.toChunks . Binary.encode

{-------------------------------------------------------------------------------
  Internal auxiliary: async
-------------------------------------------------------------------------------}

-- | Generalization of 'asyncWithUnmask'
asyncFinally ::
     MVar [Async a]
     -- ^ Registry to add the new 'Async' to
     --
     -- This is similar to the @Warden@ concept in recent versions of @async@,
     -- but we do not remove the 'Async' from the registry when it completes.
  -> ((forall b . IO b -> IO b) -> IO a)
     -- ^ Body of the new thread
  -> (Either SomeException a -> IO ())
     -- ^ Cleanup handler to be run /after/ the result of the async has been
     -- recorded. This is sometimes useful to make teardown more deterministic.
     -- Exceptions thrown by the cleanup handler are silently discarded.
  -> IO (Async a)
asyncFinally registry action cleanup =
   mask_ $ fixIO $ \me -> do
     var <- newEmptyTMVarIO
     tid <- forkIOWithUnmask $ \unmask -> do
              modifyMVar_ registry $ return . (me:)
              result <- try (action unmask)
              atomically $ putTMVar var result
              cleanup result `catch` \(_e :: SomeException) -> return ()
     return (Async.Internal.Async tid (readTMVar var))
