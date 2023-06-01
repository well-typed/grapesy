-- | Open (ongoing) RPC call
--
-- Intended for uqnqualified import.
module Network.GRPC.Client.Call (
    -- * Definition
    Call -- opaque

    -- * Construction
  , initiateCall
  , abortCall

    -- * Query
  , callResponseMetadata

    -- * Open (ongoing) call
  , sendInput
  , recvOutput

    -- ** Protocol specific wrappers
  , sendOnlyInput
  , sendAllInputs
  , recvOnlyOutput
  , recvAllOutputs
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Tracer
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteString.Lazy qualified as Lazy
import Data.Maybe (fromMaybe)
import GHC.Stack
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Client.Connection (Connection, ConnParams (..))
import Network.GRPC.Client.Connection qualified as Connection
import Network.GRPC.Common.Compression (Compression(..), CompressionId)
import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Common.Exceptions
import Network.GRPC.Spec
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.LengthPrefixed qualified as LengthPrefixed
import Network.GRPC.Spec.PseudoHeaders
import Network.GRPC.Spec.Request qualified as Request
import Network.GRPC.Spec.Response qualified as Response
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Peer (Peer, ServerContext)
import Network.GRPC.Util.Peer qualified as Peer
import Network.GRPC.Util.StreamElem

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | State of the call
--
-- This type is kept abstract (opaque) in the public facing API.
data Call rpc = IsRPC rpc => Call {
      callRPC  :: rpc
    , callPeer :: Peer (Input rpc) (Output rpc)

      -- | The initial metadata that was included in the response headers
      --
      -- The server might send additional metadata after the final output;
      -- see 'recvOutput'.
      --
      -- This lives in STM because it might block: we need to wait until we
      -- receive the metadata. The precise communication pattern will depend
      -- on the specifics of each server, but
      --
      -- * It might well be necessary to send one or more inputs to the server
      --   before it returns any replies
      -- * The response metadata /will/ be available before the first output
      --   from the server, and may indeed be available /well/ before.
    , callResponseMetadata :: STM [CustomMetadata]
    }

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

getRequestHeaders :: Connection -> CallParams -> IO RequestHeaders
getRequestHeaders conn callParams = do
    meta <- Connection.currentMeta conn
    return RequestHeaders{
        requestParams = CallParams {
            callTimeout =
              asum [
                  callTimeout callParams
                , connDefaultTimeout $ Connection.params conn
                ]
          , callRequestMetadata =
              callRequestMetadata callParams
          }
      , requestCompression =
           fromMaybe Compression.identity $
             Connection.serverCompression meta
      , requestAcceptCompression =
          Compression.offer $
            connCompression $ Connection.params conn
      }

initiateCall :: forall rpc.
     IsRPC rpc
  => Connection -> CallParams -> rpc -> IO (Call rpc)
initiateCall conn perCallParams rpc = do
    requestHeaders <- getRequestHeaders conn perCallParams
    let resourceHeaders :: RawResourceHeaders
        resourceHeaders = buildResourceHeaders $ ResourceHeaders {
              resourceMethod = Post
            , resourcePath   = rpcPath rpc
            }

    -- The server and the client can make independent choices about which
    -- compression algorithm to use. The server tells us which algorithm they
    -- use in the response headers; we make our choice here (that should will
    -- have been informed by which algorithms the server supports, of course).
    let cOut :: Compression
        cOut = requestCompression requestHeaders

        peerConfig :: Peer.Config Compression (Input rpc) (Output rpc)
        peerConfig = Peer.Config {
              serializeOutbound =         LengthPrefixed.buildInput  rpc cOut
            , parseInbound      = \cIn -> LengthPrefixed.parseOutput rpc cIn
            }

    responseMetadataVar <- newEmptyTMVarIO

    callPeer <-
      Peer.connectToServer
        tracer
        (Connection.send conn)
        peerConfig
        Peer.Server {
            serverContext  = processHeaders conn rpc responseMetadataVar
          , requestMethod  = rawMethod resourceHeaders
          , requestPath    = rawPath resourceHeaders
          , requestHeaders = Request.buildHeaders requestHeaders rpc
          }

    return Call{
        callRPC = rpc
      , callPeer
      , callResponseMetadata = readTMVar responseMetadataVar
      }
  where
    tracer :: Tracer IO (Peer.DebugMsg (Input rpc) (Output rpc))
    tracer =
        contramap (SomeRPC . Connection.PeerDebugMsg @rpc) $
          connTracer (Connection.params conn)

{-------------------------------------------------------------------------------
  Closing an open RPC
-------------------------------------------------------------------------------}

-- | Abort an open RPC call
--
-- TODO: Docs.
abortCall :: Exception e => Call rpc -> HasCallStack => e -> IO ()
abortCall Call{callPeer} = Peer.disconnect callPeer

{-------------------------------------------------------------------------------
  Open (ongoing) call
-------------------------------------------------------------------------------}

-- | Send an input to the peer
--
-- This lives in @STM@ for improved composability. For example, if the peer is
-- currently busy then 'sendInput' will block, but you can then use 'orElse' to
-- provide an alternative codepath.
--
-- Calling 'sendInput' again after sending the final message is a bug.
--
-- WARNING: Sending multiple messages on the same call within the same STM
-- transaction will deadlock; you should enqueue them separately.
--
-- TODO: We should have a way to detect this and throw an exception.
sendInput :: HasCallStack => Call rpc -> StreamElem () (Input rpc) -> STM ()
sendInput Call{callPeer} = Peer.send callPeer . first (const [])

-- | Receive an output from the peer
--
-- This lives in @STM@ for improved compositionality. For example, you can wait
-- on multiple clients and see which one responds first.
--
-- After the final 'Output', you will receive any 'CustomMetadata' (application
-- defined trailers) that the server returns. We do /NOT/ include the
-- 'GrpcStatus' here: a status of 'GrpcOk' carries no information, and any other
-- status will result in a 'GrpcException'. Calling 'recvOutput' again after
-- receiving the trailers is a bug and results in a 'RecvAfterFinal' exception.
recvOutput :: Call rpc -> STM (StreamElem [CustomMetadata] (Output rpc))
recvOutput Call{callRPC, callPeer} =
        Peer.recv callPeer
    >>= bitraverse (processTrailers callRPC) pure

{-------------------------------------------------------------------------------
  Protocol specific wrappers
-------------------------------------------------------------------------------}

sendOnlyInput ::
     MonadIO m
  => Call rpc
  -> Input rpc
  -> m ()
sendOnlyInput call input = liftIO $
    atomically $ sendInput call (FinalElem input ())

sendAllInputs :: forall m rpc.
     MonadIO m
  => Call rpc
  -> m (StreamElem () (Input rpc))
  -> m ()
sendAllInputs call produceInput = loop
  where
    loop :: m ()
    loop = do
        inp <- produceInput
        liftIO $ atomically $ sendInput call inp
        case streamElemDefinitelyFinal inp of
          Nothing -> loop
          Just _  -> return ()

-- | Receive output, which we expect to be the /only/ output
--
-- Throws 'ProtocolException' if the server does not send precisely one output.
recvOnlyOutput ::
     MonadIO m
  => Call rpc
  -> m (Output rpc, [CustomMetadata])
recvOnlyOutput call@Call{callRPC} = liftIO $ do
    out1 <- atomically $ recvOutput call
    case out1 of
      NoMoreElems    ts -> protocolException callRPC $ TooFewOutputs ts
      FinalElem  out ts -> return (out, ts)
      StreamElem out    -> do
        out2 <- atomically $ recvOutput call
        case out2 of
          NoMoreElems ts    -> return (out, ts)
          FinalElem  out' _ -> protocolException callRPC $ TooManyOutputs out'
          StreamElem out'   -> protocolException callRPC $ TooManyOutputs out'

recvAllOutputs :: forall m rpc.
     MonadIO m
  => Call rpc
  -> (Output rpc -> m ())
  -> m [CustomMetadata]
recvAllOutputs call processOutput = loop
  where
    loop :: m [CustomMetadata]
    loop = do
        mOut <- liftIO $ atomically $ recvOutput call
        case mOut of
          StreamElem out -> do
            processOutput out
            loop
          NoMoreElems trailers ->
            return trailers
          FinalElem out trailers -> do
            processOutput out
            return trailers

{-------------------------------------------------------------------------------
  Response headers
-------------------------------------------------------------------------------}

-- | Process response headers
--
-- The gRPC specification defines
--
-- > Response → (Response-Headers *Length-Prefixed-Message Trailers)
-- >          / Trailers-Only
--
-- The spec states in prose that
--
-- 1. For responses end-of-stream is indicated by the presence of the END_STREAM
--    flag on the last received HEADERS frame that carries Trailers.
-- 2. Implementations should expect broken deployments to send non-200 HTTP
--    status codes in responses as well as a variety of non-GRPC content-types
--    and to omit Status & Status-Message. Implementations must synthesize a
--    Status & Status-Message to propagate to the application layer when this
--    occurs.
--
-- This means the only reliable way to determine if we're in the @Trailers-Only@
-- case is to see if the stream is terminated after we received the \"trailers\"
-- (headers). We don't get direct access to this information from @http2@, /but/
-- we are told indirectly: if the stream is terminated, we are told that the
-- body size is 0 (normally we do not expect an indication of body size at all);
-- this is independent from any content-length header (which gRPC does not in
-- fact allow for).
--
-- Moreover, the gRPC spec states that
--
-- > Trailers-Only is permitted for calls that produce an immediate error
--
-- However, in practice gRPC servers can also respond with @Trailers-Only@ in
-- non-error cases, simply indicating that the server considers the conversation
-- over. To distinguish, we look at 'trailerGrpcStatus': in case of 'GrpcOk' we
-- return the 'Trailers', and in the case of 'GrpcError' we throw
-- 'RpcImmediateError'.
processHeaders ::
     IsRPC rpc
  => Connection
  -> rpc
  -> TMVar [CustomMetadata] -- ^ Response metadata
  -> Maybe HTTP.Status
  -> Maybe Int
  -> [HTTP.Header]
  -> IO (ServerContext Compression)
processHeaders conn rpc responseMetadataVar status contentLength headers = do
    case HTTP.statusCode <$> status of
      Just 200   -> return ()
      _otherwise -> throwIO $ ResponseInvalidStatus status

    case contentLength of
      Just 0 -> return Peer.NoServerContext
      Just l -> throwIO $ ResponseUnexpectedContentLength l
      Nothing -> do
        -- The common @Response@ case (i.e., not @Trailers-Only@)
        hdrs <- case Response.parseHeaders rpc headers of
                  Left  err    -> throwIO $ ResponseInvalidHeaders err
                  Right parsed -> return parsed
        atomically $ putTMVar responseMetadataVar $ responseMetadata hdrs
        meta <- Connection.updateMeta conn hdrs
        return $ Peer.ServerContext $
          fromMaybe Compression.identity (Connection.serverCompression meta)

processTrailers ::
     (IsRPC rpc, MonadThrow m)
  => rpc -> [HTTP.Header] -> m [CustomMetadata]
processTrailers rpc raw = do
    case Response.parseTrailers rpc raw of
      Left err -> throwM $ ResponseInvalidTrailers err
      Right trailers ->
        case grpcExceptionFromTrailers trailers of
          Nothing  -> return $ trailerMetadata trailers
          Just err -> throwM err

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Server sent an invalid response
--
-- If this happens, it's indicative of a bad server (or a bug in @grapesy@..).
-- It is /not/ indicative of a bug in client code.
data InvalidResponse =
    -- | Server sent a HTTP status other than 200 OK
    --
    -- The spec explicitly disallows this; if a particular method is not
    -- supported, then the server will respond with HTTP 200 and then a
    -- grpc-status of 'GrpcUnimplemented'.
    ResponseInvalidStatus (Maybe HTTP.Status)

    -- | gRPC does not support the HTTP2 content-length header
  | ResponseUnexpectedContentLength Int

    -- | We received a response header we could not parse
  | ResponseInvalidHeaders String

    -- | We received a trailing header we could not parse
  | ResponseInvalidTrailers String

    -- | When the server indicated end of stream, we still had unparsed data
  | ResponseTrailingData Lazy.ByteString

    -- | We failed to parse an incoming message
  | ResponseParseError String

    -- | Server used an unexpected compression algorithm
  | ResponseUnexpectedCompression CompressionId
  deriving stock (Show)
  deriving anyclass (Exception)

