module Server (main) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Text qualified as Text
import System.Directory
import System.IO

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

import Proto.API.Fileserver

{-------------------------------------------------------------------------------
  Individual handlers
-------------------------------------------------------------------------------}

-- Note: this just allows to download any file, not intended to be secure!
download :: Call (Protobuf Fileserver "download") -> IO ()
download call = do
    req :: Proto File <- recvFinalInput call
    let fp :: FilePath
        fp = Text.unpack (req ^. #name)

    fileSize <- getFileSize fp
    modTime  <- getModificationTime fp
    setResponseInitialMetadata call $ DownloadStart fileSize (Just modTime)
    initiateResponse call

    withFile fp ReadMode $ \h -> do
      let loop :: SHA256.Ctx -> IO ()
          loop ctx = do
              chunk <- BS.hGet h defaultChunkSize
              eof   <- hIsEOF h

              let resp :: Proto Partial
                  resp = defMessage & #chunk .~ chunk

                  ctx' :: SHA256.Ctx
                  ctx' = SHA256.update ctx chunk

              if eof then
                sendFinalOutput call (resp, DownloadDone $ SHA256.finalize ctx')
              else do
                sendNextOutput call resp
                loop ctx'

      loop SHA256.init

{-------------------------------------------------------------------------------
  Server top-level
-------------------------------------------------------------------------------}

methods :: Methods IO (ProtobufMethodsOf Fileserver)
methods =
      RawMethod (mkRpcHandlerNoDefMetadata download)
    $ NoMoreMethods


main :: IO ()
main =
    runServerWithHandlers def config $ fromMethods methods
  where
    config :: ServerConfig
    config = ServerConfig {
          serverInsecure = Just (InsecureConfig (Just "0.0.0.0") defaultInsecurePort)
        , serverSecure   = Nothing
        }
