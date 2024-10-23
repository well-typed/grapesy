{-# LANGUAGE OverloadedStrings #-}

-- | Open (ongoing) RPC call
--
-- Intended for uqnqualified import.
module Network.GRPC.Server.Call (
    Call(..)

    -- * Construction
  , setupCall

    -- * Open (ongoing) call
  , recvInput
  , sendOutput
  , sendGrpcException
  , getRequestMetadata
  , setResponseInitialMetadata

    -- ** Protocol specific wrappers
  , sendNextOutput
  , sendFinalOutput
  , sendTrailers
  , recvNextInput
  , recvFinalInput
  , recvEndOfInput

    -- ** Low-level\/specialized API
  , initiateResponse
  , sendTrailersOnly
  , recvNextInputElem
  , recvInputWithMeta
  , sendOutputWithMeta
  , getRequestHeaders

    -- ** Internal API
  , sendProperTrailers
  , serverExceptionToClientError

    -- * Exceptions
  , HandlerTerminated(..)
  , ResponseAlreadyInitiated(..)
  ) where

import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.Catch
import Data.Bitraversable
import Data.List.NonEmpty (NonEmpty)
import Data.Void
import GHC.Stack
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.Headers
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server.Context
import Network.GRPC.Server.Session
import Network.GRPC.Spec
import Network.GRPC.Spec.Serialization
import Network.GRPC.Util.HTTP2 (fromHeaderTable)
import Network.GRPC.Util.Session qualified as Session
import Network.GRPC.Util.Session.Server qualified as Server

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Open connection to a client
data Call rpc = SupportsServerRpc rpc => Call {
      -- | Server context (across all calls)
      callContext :: ServerContext

      -- | Server session (for this call)
    , callSession :: ServerSession rpc

      -- | Bidirectional channel to the client
    , callChannel :: Session.Channel (ServerSession rpc)

      -- | Request headers
    , callRequestHeaders :: RequestHeaders' HandledSynthesized

      -- | Response metadata
      --
      -- Metadata to be included when we send the initial response headers.
      --
      -- Can be updated until the first message (see 'callFirstMessage'), at
      -- which point it /must/ have been set (if not, an exception is thrown).
    , callResponseMetadata :: TVar (Maybe (ResponseInitialMetadata rpc))

      -- | What kicked off the response?
      --
      -- This is empty until the response /has/ in fact been kicked off.
    , callResponseKickoff :: TMVar Kickoff
    }

-- | What kicked off the response?
--
-- When the server handler starts, we do not immediately initiate the response
-- to the client, because the server might not have decided the initial response
-- metadata yet (indeed, it might need wait for some incoming messages from the
-- client before it can compute that metadata). We therefore wait until we
-- send the initial response headers, until the handler..
--
-- 1. explicitly calls 'initiateResponse'
-- 2. sends the first message using 'sendOutput'
-- 3. initiates and immediately terminates the response using 'sendTrailersOnly'
--
-- We only need distinguish between (1 or 2) versus (3), corresponding precisely
-- to the two constructors of 'FlowStart'.
--
-- We record the 'CallStack' of the call that initiated the response.
data Kickoff =
    KickoffRegular      CallStack
  | KickoffTrailersOnly CallStack TrailersOnly
  deriving (Show)

kickoffCallStack :: Kickoff -> CallStack
kickoffCallStack (KickoffRegular      cs  ) = cs
kickoffCallStack (KickoffTrailersOnly cs _) = cs

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

-- | Setup call
--
-- No response is sent to the caller.
--
-- May throw 'CallSetupFailure'.
setupCall :: forall rpc.
     SupportsServerRpc rpc
  => Server.ConnectionToClient
  -> ServerContext
  -> IO (Call rpc, Maybe Timeout)
setupCall conn callContext@ServerContext{serverParams} = do
    callResponseMetadata <- newTVarIO Nothing
    callResponseKickoff  <- newEmptyTMVarIO

    (inboundHeaders, timeout) <- determineInbound callSession req
    let callRequestHeaders = inbHeaders inboundHeaders

    -- Technically compression is only relevant in the 'KickoffRegular' case
    -- (i.e., in the case that the /server/ responds with messages, rather than
    -- make use the Trailers-Only case). However, the kickoff might not be known
    -- until the server has received various messages from the client. To
    -- simplify control flow, therefore, we do compression negotation early.
    let cOut = getOutboundCompression callSession $
                 requestAcceptCompression callRequestHeaders

    callChannel :: Session.Channel (ServerSession rpc) <-
      Session.setupResponseChannel
        callSession
        conn
        (Session.FlowStartRegular inboundHeaders)
        ( startOutbound
            serverParams
            callResponseMetadata
            callResponseKickoff
            cOut
        )

    return (
        Call{
            callContext
          , callSession
          , callRequestHeaders
          , callResponseMetadata
          , callResponseKickoff
          , callChannel
          }
      , timeout
      )
  where
    callSession :: ServerSession rpc
    callSession = ServerSession {
          serverSessionContext = callContext
        }

    req :: HTTP2.Request
    req = Server.request conn

-- | Parse inbound headers
determineInbound :: forall rpc.
     SupportsServerRpc rpc
  => ServerSession rpc
  -> HTTP2.Request
  -> IO (Headers (ServerInbound rpc), Maybe Timeout)
determineInbound session req = do
    requestHeaders' <- throwSynthesized throwIO parsed
    case verifyAllIf serverVerifyHeaders requestHeaders' of
      Left  err  -> throwIO $ CallSetupInvalidRequestHeaders err
      Right hdrs -> do
        cIn <- getInboundCompression session (requiredRequestCompression hdrs)
        return (
            InboundHeaders {
                inbHeaders     = requestHeaders'
              , inbCompression = cIn
              }
          , requiredRequestTimeout hdrs
          )
  where
    ServerSession{serverSessionContext} = session
    ServerContext{serverParams} = serverSessionContext
    ServerParams{serverVerifyHeaders} = serverParams

    parsed :: RequestHeaders' GrpcException
    parsed = parseRequestHeaders' (Proxy @rpc) $
               fromHeaderTable $ HTTP2.requestHeaders req

-- | Determine outbound flow start
--
-- This blocks until the 'Kickoff' is known.
startOutbound :: forall rpc.
     SupportsServerRpc rpc
  => ServerParams
  -> TVar (Maybe (ResponseInitialMetadata rpc))
  -> TMVar Kickoff
  -> Compression
  -> IO (Session.FlowStart (ServerOutbound rpc), Session.ResponseInfo)
startOutbound serverParams metadataVar kickoffVar cOut = do
    -- Wait for kickoff (see 'Kickoff' for discussion)
    kickoff <- atomically $ readTMVar kickoffVar

    -- Get response metadata (see 'setResponseMetadata')
    -- It is important we read this only /after/ the kickoff.
    responseMetadata <- do
      mMetadata <- atomically $ readTVar metadataVar
      case mMetadata of
        Just md -> buildMetadataIO md
        Nothing -> throwIO $ ResponseInitialMetadataNotSet

    -- Session start
    let flowStart :: Session.FlowStart (ServerOutbound rpc)
        flowStart =
          case kickoff of
            KickoffRegular _cs ->
              Session.FlowStartRegular $ OutboundHeaders {
                  outHeaders = ResponseHeaders {
                      responseCompression =
                        Just $ Compr.compressionId cOut
                    , responseAcceptCompression =
                        Just $ Compr.offer compr
                    , responseContentType =
                        serverContentType serverParams
                    , responseMetadata =
                        customMetadataMapFromList responseMetadata
                    , responseUnrecognized =
                        ()
                    }
                , outCompression = cOut
                }
            KickoffTrailersOnly _cs trailers ->
              Session.FlowStartNoMessages trailers

    return (flowStart, buildResponseInfo flowStart)
  where
    compr :: Compr.Negotation
    compr = serverCompression serverParams

    buildResponseInfo ::
         Session.FlowStart (ServerOutbound rpc)
      -> Session.ResponseInfo
    buildResponseInfo start = Session.ResponseInfo {
          responseStatus  = HTTP.ok200
        , responseHeaders =
            case start of
              Session.FlowStartRegular headers ->
                buildResponseHeaders (Proxy @rpc) (outHeaders headers)
              Session.FlowStartNoMessages trailers ->
                buildTrailersOnly (Proxy @rpc) trailers
        , responseBody = Nothing
        }

-- | Determine compression used by the peer for messages to us
getInboundCompression ::
     ServerSession rpc
  -> Maybe CompressionId
  -> IO Compression
getInboundCompression session = \case
    Nothing  -> return noCompression
    Just cid ->
      case Compr.getSupported serverCompression cid of
        Just compr -> return compr
        Nothing    -> throwIO $ CallSetupUnsupportedCompression cid
  where
    ServerSession{serverSessionContext} = session
    ServerContext{serverParams} = serverSessionContext
    ServerParams{serverCompression} = serverParams

-- | Determine compression to be used for messages to the peer
--
-- In the case that we fail to parse the @grpc-accept-encoding@ header, we
-- simply use no compression.
getOutboundCompression ::
     ServerSession rpc
  -> Either (InvalidHeaders HandledSynthesized) (Maybe (NonEmpty CompressionId))
  -> Compression
getOutboundCompression session = \case
    Left _invalidHeader -> noCompression
    Right Nothing       -> noCompression
    Right (Just cids)   -> Compr.choose serverCompression cids
  where
    ServerSession{serverSessionContext} = session
    ServerContext{serverParams} = serverSessionContext
    ServerParams{serverCompression} = serverParams

-- | Turn exception raised in server handler to error to be sent to the client
serverExceptionToClientError :: ServerParams -> SomeException -> IO ProperTrailers
serverExceptionToClientError params err
    | Just (err' :: GrpcException) <- fromException err =
        return $ grpcExceptionToTrailers err'
    | otherwise = do
        mMsg <- serverExceptionToClient params err
        return $ simpleProperTrailers (GrpcError GrpcUnknown) mMsg mempty

{-------------------------------------------------------------------------------
  Open (ongoing) call
-------------------------------------------------------------------------------}

-- | Receive RPC input from the client
--
-- We do not return trailers, since gRPC does not support sending trailers from
-- the client to the server (only from the server to the client).
recvInput :: HasCallStack => Call rpc -> IO (StreamElem NoMetadata (Input rpc))
recvInput = fmap (fmap snd) . recvInputWithMeta

-- | Receive RPC input from the client, if one exists
--
-- When using `recvInput`, the final few messages can look like
--
-- > ..
-- > StreamElem msg2
-- > StreamElem msg1
-- > FinalElem  msg0 ..
--
-- or like
--
-- > ..
-- > StreamElem msg2
-- > StreamElem msg1
-- > StreamElem msg0
-- > NoMoreElems ..
--
-- depending on whether the client indicates that @msg0@ is the last message
-- when it sends it, or indicates end-of-stream only /after/ sending the last
-- message.
--
-- Many applications do not need to distinguish between these two cases, but
-- the API provided by `recvInput` makes it a bit awkward to treat them the
-- same, especially since it is an error to call `recvInput` again after
-- receiving either `FinalElem` or `NoMoreElems`. In this case, it may be more
-- convenient to use `recvNextInputElem`, which will report both cases as
--
-- > ..
-- > NextElem msg2
-- > NextElem msg1
-- > NextElem msg0
-- > NoNextElem
recvNextInputElem :: HasCallStack => Call rpc -> IO (NextElem (Input rpc))
recvNextInputElem = fmap (fmap snd) . recvEither

-- | Generalization of 'recvInput', providing additional meta-information
--
-- This can be used to get some information about how the message was sent, such
-- as its compressed and uncompressed size.
--
-- Most applications will never need to use this function.
recvInputWithMeta :: forall rpc.
     HasCallStack
  => Call rpc
  -> IO (StreamElem NoMetadata (InboundMeta, Input rpc))
recvInputWithMeta = recvBoth

-- | Send RPC output to the client
--
-- This will send a 'GrpcStatus' of 'GrpcOk' to the client; for anything else
-- (i.e., to indicate something went wrong), the server handler should call
-- 'sendGrpcException'.
--
-- This is a blocking call if this is the final message (i.e., the call will not
-- return until the message has been written to the HTTP2 stream).
sendOutput ::
     HasCallStack
  => Call rpc
  -> StreamElem (ResponseTrailingMetadata rpc) (Output rpc) -> IO ()
sendOutput call = sendOutputWithMeta call . fmap (def,)

-- | Generalization of 'sendOutput' with additional control
--
-- This can be used for example to enable or disable compression for individual
-- messages.
--
-- Most applications will never need to use this function.
sendOutputWithMeta :: forall rpc.
     HasCallStack
  => Call rpc
  -> StreamElem (ResponseTrailingMetadata rpc) (OutboundMeta, Output rpc)
  -> IO ()
sendOutputWithMeta call@Call{callChannel} msg = do
    initiateResponse call
    msg' <- bitraverse mkTrailers return msg
    Session.send callChannel msg'

    -- This /must/ be called before leaving the scope of 'runHandler' (or we
    -- risk that the HTTP2 stream is cancelled). We can't call 'waitForOutbound'
    -- /in/ 'runHandler', because if the handler for whatever reason never
    -- writes the final message, such a call would block indefinitely.
    StreamElem.whenDefinitelyFinal msg $ \_ ->
      Session.waitForOutbound callChannel
  where
    mkTrailers :: ResponseTrailingMetadata rpc -> IO ProperTrailers
    mkTrailers metadata = do
        metadata' <- customMetadataMapFromList <$> buildMetadataIO metadata
        return $ simpleProperTrailers GrpcOk Nothing metadata'

-- | Send 'GrpcException' to the client
--
-- This closes the connection to the client; sending further messages will
-- result in an exception being thrown.
--
-- Instead of calling 'sendGrpcException' handlers can also simply /throw/ the
-- gRPC exception (the @grapesy@ /client/ API treats this the same way: a
-- 'GrpcStatus' other than 'GrpcOk' will be raised as a 'GrpcException'). The
-- difference is primarily one of preference/convenience, but the two are not
-- /completely/ the same: when the 'GrpcException' is thrown,
-- 'Context.serverTopLevel' will see the handler throw an exception (and, by
-- default, log that exception); when using 'sendGrpcException', the handler is
-- considered to have terminated normally. For handlers defined using
-- "Network.GRPC.Server.StreamType" throwing the exception is the only option.
--
-- Technical note: if the response to the client has not yet been initiated when
-- 'sendGrpcException' is called, this will make use of the [gRPC
-- Trailers-Only](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md)
-- case.
sendGrpcException :: Call rpc -> GrpcException -> IO ()
sendGrpcException call = sendProperTrailers call . grpcExceptionToTrailers

-- | Get request metadata
--
-- The request metadata is included in the client's request headers when they
-- first make the request, and is therefore available immediately to the handler
-- (even if the first /message/ from the client may not yet have been sent).
--
-- NOTE: If the client sends custom metadata that we cannot parse (perhaps it
-- uses a reserved name, or the binary encoding is invalid, etc.), they will not
-- be included here. If you want access to these headers, use
-- 'getRequestHeaders' and then extract 'requestUnrecognized'.
getRequestMetadata :: Call rpc -> IO (RequestMetadata rpc)
getRequestMetadata Call{callRequestHeaders} =
    parseMetadata . customMetadataMapToList $
      requestMetadata callRequestHeaders

-- | Set the initial response metadata
--
-- This can be set at any time before the response is initiated (either
-- implicitly by calling 'sendOutput', or explicitly by calling
-- 'initiateResponse' or 'sendTrailersOnly'). If the response has already
-- been initiated (and therefore the initial response metadata already sent),
-- will throw 'ResponseAlreadyInitiated'.
--
-- Note that this is about the /initial/ metadata; additional metadata can be
-- sent after the final message; see 'sendOutput'.
setResponseInitialMetadata ::
     HasCallStack
  => Call rpc -> ResponseInitialMetadata rpc -> IO ()
setResponseInitialMetadata Call{ callResponseMetadata
                               , callResponseKickoff
                               }
                           md = atomically $ do
    mKickoff <- fmap kickoffCallStack <$> tryReadTMVar callResponseKickoff
    case mKickoff of
      Nothing -> writeTVar callResponseMetadata (Just md)
      Just cs -> throwSTM $ ResponseAlreadyInitiated cs callStack

{-------------------------------------------------------------------------------
  Low-level API
-------------------------------------------------------------------------------}

-- | Initiate the response
--
-- This will cause the initial response metadata to be sent
-- (see also 'setResponseMetadata').
--
-- Does nothing if the response was already initated (that is, the response
-- headers, or trailers in the case of 'sendTrailersOnly', have already been
-- sent).
initiateResponse :: HasCallStack => Call rpc -> IO ()
initiateResponse Call{callResponseKickoff} = void $
    atomically $ tryPutTMVar callResponseKickoff $ KickoffRegular callStack

-- | Use the gRPC @Trailers-Only@ case for non-error responses
--
-- Under normal circumstances a gRPC server will respond to the client with
-- an initial set of headers, then zero or more messages, and finally a set of
-- trailers. When there /are/ no messages, this /can/ be collapsed into a single
-- set of trailers (or headers, depending on your point of view); the gRPC
-- specification refers to this as the @Trailers-Only@ case. It mandates:
--
-- > Most responses are expected to have both headers and trailers but
-- > Trailers-Only is permitted for calls that produce an immediate error.
--
-- In @grapesy@, if a server handler throws a 'GrpcException', we will make use
-- of this @Trailers-Only@ case if applicable, as per the specification.
--
-- /However/, some servers make use of @Trailers-Only@ also in non-error cases.
-- For example, the @listFeatures@ handler in the official Python route guide
-- example server will use @Trailers-Only@ if there are no features to report.
-- Since this is not conform the gRPC specification, we do not do this in
-- @grapesy@ by default, but we make the option available through
-- 'sendTrailersOnly'.
--
-- Throws 'ResponseAlreadyInitiated' if the response has already been initiated.
sendTrailersOnly ::
     HasCallStack
  => Call rpc -> ResponseTrailingMetadata rpc -> IO ()
sendTrailersOnly Call{callContext, callResponseKickoff} metadata = do
    metadata' <- buildMetadataIO metadata
    atomically $ do
      previously <- fmap kickoffCallStack <$> tryReadTMVar callResponseKickoff
      case previously of
        Nothing -> putTMVar callResponseKickoff $
                     KickoffTrailersOnly callStack (trailers metadata')
        Just cs -> throwSTM $ ResponseAlreadyInitiated cs callStack
  where
    ServerContext{serverParams} = callContext

    trailers :: [CustomMetadata] -> TrailersOnly
    trailers metadata' = TrailersOnly {
          trailersOnlyContentType = serverContentType serverParams
        , trailersOnlyProper      = simpleProperTrailers GrpcOk Nothing $
                                      customMetadataMapFromList metadata'
        }

-- | Get full request headers, including any potential invalid headers
--
-- NOTE: When 'serverVerifyHeaders' is enabled the caller can be sure that the
-- 'RequestHeaders'' do not contain any errors, even though unfortunately this
-- is not visible from the type.
getRequestHeaders :: Call rpc -> IO (RequestHeaders' HandledSynthesized)
getRequestHeaders Call{callRequestHeaders} =
    return callRequestHeaders

{-------------------------------------------------------------------------------
  Protocol specific wrappers
-------------------------------------------------------------------------------}

-- | Send the next output
--
-- If this is the last output, you should call 'sendTrailers' after
-- (or use 'sendFinalOutput').
sendNextOutput ::
     HasCallStack
  => Call rpc -> Output rpc -> IO ()
sendNextOutput call = sendOutput call . StreamElem

-- | Send final output
--
-- See also 'sendTrailers'.
sendFinalOutput ::
     HasCallStack
  => Call rpc -> (Output rpc, ResponseTrailingMetadata rpc) -> IO ()
sendFinalOutput call = sendOutput call . uncurry FinalElem

-- | Send trailers
--
-- This tells the client that there will be no more outputs. You should call
-- this (or 'sendFinalOutput') even when there is no special information to be
-- included in the trailers.
sendTrailers ::
     HasCallStack
  => Call rpc -> ResponseTrailingMetadata rpc -> IO ()
sendTrailers call = sendOutput call . NoMoreElems

-- | Receive next input
--
-- Throws 'ProtocolException' if there are no more inputs.
recvNextInput :: forall rpc. HasCallStack => Call rpc -> IO (Input rpc)
recvNextInput call@Call{} = do
    mInp <- recvNextInputElem call
    case mInp of
      NoNextElem   -> err $ TooFewInputs @rpc
      NextElem inp -> return inp
  where
    err :: ProtocolException rpc -> IO a
    err = throwIO . ProtocolException

-- | Receive input, which we expect to be the /final/ input
--
-- Throws 'ProtocolException' if the input we receive is not final.
--
-- NOTE: If the first input we receive from the client is not marked as final,
-- we will block until we receive the end-of-stream indication.
recvFinalInput :: forall rpc. HasCallStack => Call rpc -> IO (Input rpc)
recvFinalInput call@Call{} = do
    inp1 <- recvInput call
    case inp1 of
      NoMoreElems    NoMetadata -> err $ TooFewInputs @rpc
      FinalElem  inp NoMetadata -> return inp
      StreamElem inp            -> do
        inp2 <- recvInput call
        case inp2 of
          NoMoreElems     NoMetadata -> return inp
          FinalElem  inp' NoMetadata -> err $ TooManyInputs @rpc inp'
          StreamElem inp'            -> err $ TooManyInputs @rpc inp'
  where
    err :: ProtocolException rpc -> IO a
    err = throwIO . ProtocolException

-- | Wait for the client to indicate that there are no more inputs
--
-- Throws 'ProtocolException' if we received an input.
recvEndOfInput :: forall rpc. HasCallStack => Call rpc -> IO ()
recvEndOfInput call@Call{} = do
    mInp <- recvInput call
    case mInp of
      NoMoreElems    NoMetadata -> return ()
      FinalElem  inp NoMetadata -> err $ TooManyInputs @rpc inp
      StreamElem inp            -> err $ TooManyInputs @rpc inp
  where
    err :: ProtocolException rpc -> IO a
    err = throwIO . ProtocolException

{-------------------------------------------------------------------------------
  Internal API
-------------------------------------------------------------------------------}

-- | Send 'ProperTrailers'
--
-- This function is not part of the public API: we use it as the top-level
-- exception handler in "Network.GRPC.Server" to forward exceptions in server
-- handlers to the client.
--
-- If no messages have been sent yet, we make use of the @Trailers-Only@ case.
sendProperTrailers :: Call rpc -> ProperTrailers -> IO ()
sendProperTrailers Call{callContext, callResponseKickoff, callChannel}
                   trailers = do
    updated <-
      atomically $
        tryPutTMVar callResponseKickoff $
          KickoffTrailersOnly
            callStack
            ( properTrailersToTrailersOnly (
                trailers
              , serverContentType serverParams
              )
            )
    unless updated $
      -- If we didn't update, then the response has already been initiated and
      -- we cannot make use of the Trailers-Only case.
      Session.send callChannel (NoMoreElems trailers)
    Session.waitForOutbound callChannel
  where
    ServerContext{serverParams} = callContext

{-------------------------------------------------------------------------------
  Internal auxiliary: deal with final message

  We get "trailers" (really headers) only in the Trailers-Only case.
-------------------------------------------------------------------------------}

recvBoth :: forall rpc.
     HasCallStack
  => Call rpc
  -> IO (StreamElem NoMetadata (InboundMeta, Input rpc))
recvBoth Call{callChannel} =
    flatten <$> Session.recvBoth callChannel
  where
    -- We lose type information here: Trailers-Only is no longer visible
    flatten ::
         Either
           Void
           (StreamElem NoMetadata (InboundMeta, Input rpc))
      -> StreamElem NoMetadata (InboundMeta, Input rpc)
    flatten (Left  impossible) = absurd impossible
    flatten (Right streamElem) = streamElem

recvEither :: forall rpc.
     HasCallStack
  => Call rpc
  -> IO (NextElem (InboundMeta, Input rpc))
recvEither Call{callChannel} =
    flatten <$> Session.recvEither callChannel
  where
    -- We use this only in 'recvNextInputElem', where we just care that we receive
    -- a message.
    flatten ::
         Either
           Void
           (Either NoMetadata (InboundMeta, Input rpc))
      -> NextElem (InboundMeta, Input rpc)
    flatten (Left impossible)         = absurd impossible
    flatten (Right (Left NoMetadata)) = NoNextElem
    flatten (Right (Right msg))       = NextElem msg

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Handler terminated early
--
-- This gets thrown in the handler, and sent to the client, when the handler
-- terminates before sending the final output and trailers.
data HandlerTerminated = HandlerTerminated
  deriving stock (Show)
  deriving anyclass (Exception)

data ResponseAlreadyInitiated = ResponseAlreadyInitiated {
      -- | When was the response first initiated?
      responseInitiatedFirst :: CallStack

      -- | Where did we attempt to initiate the response a second time?
    , responseInitiatedAgain :: CallStack
    }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | The response initial metadata must be set before the first message is sent
data ResponseInitialMetadataNotSet = ResponseInitialMetadataNotSet
  deriving stock (Show)
  deriving anyclass (Exception)