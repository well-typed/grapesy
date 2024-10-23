module Client (main) where

import Control.Monad.State
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import System.Environment
import System.IO

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common.StreamElem qualified as StreamElem

import Proto.API.Fileserver
import ProgressT

{-------------------------------------------------------------------------------
  RPC
-------------------------------------------------------------------------------}

processPartial ::
     Handle
  -> Proto Partial
  -> ProgressT (StateT SHA256.Ctx IO) ()
processPartial h partial = do
    liftIO $ BS.hPut h chunk
    modify $ flip SHA256.update chunk
    updateProgressBar $ BS.length chunk
  where
    chunk :: ByteString
    chunk = partial ^. #chunk

download :: Connection -> String -> String -> IO ()
download conn inp out = do
    withRPC conn def (Proxy @(Protobuf Fileserver "download")) $ \call -> do
      sendFinalInput call $ defMessage & #name .~ Text.pack inp

      -- Wait for initial metadata, telling us how big the file is
      DownloadStart{downloadSize} <- recvResponseInitialMetadata call

      -- Process each chunk
      (DownloadDone{downloadHash = theirHash}, ourHash) <-
        withFile out WriteMode $ \h ->
          flip runStateT SHA256.init . runProgressT downloadSize $
            StreamElem.whileNext_ (recvOutput call) (processPartial h)

      -- Check hash
      putStrLn $ "Hash match: " ++ show (theirHash == SHA256.finalize ourHash)

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    [inp, out] <- getArgs
    withConnection def server $ \conn -> do
      download conn inp out
  where
    server :: Server
    server = ServerInsecure $ Address "127.0.0.1" defaultInsecurePort Nothing
