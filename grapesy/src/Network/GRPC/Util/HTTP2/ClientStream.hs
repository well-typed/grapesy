module Network.GRPC.Util.HTTP2.ClientStream (
    -- ** Client API
    clientInputStream,
    clientOutputStream,
) where

import Network.GRPC.Util.Stream

import Network.HTTP2.Client qualified as Client

import Network.GRPC.Util.HTTP2 (fromHeaderTable)

{-------------------------------------------------------------------------------
  Client API
-------------------------------------------------------------------------------}

clientInputStream :: Client.Response -> IO InputStream
clientInputStream resp = do
    return InputStream {
        _getChunk =
           wrapStreamExceptionsWith ServerDisconnected $
             Client.getResponseBodyChunk' resp
      , _getTrailers =
           wrapStreamExceptionsWith ServerDisconnected $
             maybe [] fromHeaderTable <$> Client.getResponseTrailers resp
      }

-- | Construct a client 'OutputStream'
--
-- We do not wrap the members of the 'OutputStream' with
-- 'wrapStreamExceptionsWith', since we do this around the entire
-- 'sendMessageLoop'. See the comment for @outboundThread@ in
-- 'Network.GRPC.Util.Session.Client.setupRequestChannel'.
clientOutputStream :: Client.OutBodyIface -> IO OutputStream
clientOutputStream iface =
    return OutputStream {
        _writeChunk = \c ->
          Client.outBodyPush iface c
      , _writeChunkFinal = \c ->
          Client.outBodyPushFinal iface c
      , _flush =
          Client.outBodyFlush iface
      }
