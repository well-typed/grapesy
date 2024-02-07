-- | Demonstration of using a custom monad stack
module Demo.Client.API.Protobuf.CanCallRPC.Greeter (
    sayHello
  , sayHelloStreamReply
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.CanCallRPC

import Proto.Helloworld

import Demo.Common.Logging qualified as Logging

{-------------------------------------------------------------------------------
  Example custom monad
-------------------------------------------------------------------------------}

data ClientState = ClientState {
      connection :: Connection
    , logger     :: forall a. Show a => a -> IO ()
    }

newtype MyClient a = WrapMyClient {
      unwrapMyClient :: ReaderT ClientState IO a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance CanCallRPC MyClient where
  getConnection = WrapMyClient $ connection <$> ask

clientLogMsg :: Show a => a -> MyClient ()
clientLogMsg x = WrapMyClient $ ReaderT $ \state -> logger state x

runMyClient :: Connection -> MyClient a -> IO a
runMyClient conn client =
    runReaderT
      (unwrapMyClient client)
      (ClientState conn Logging.logMsg)

{-------------------------------------------------------------------------------
  Implement the Greeter API using 'MyClient'
-------------------------------------------------------------------------------}

sayHello :: Connection -> HelloRequest -> IO ()
sayHello conn name = runMyClient conn $ do
    reply <- nonStreaming (rpc @(Protobuf Greeter "sayHello")) name
    clientLogMsg reply

sayHelloStreamReply :: Connection -> HelloRequest -> IO ()
sayHelloStreamReply conn name = runMyClient conn $ do
    serverStreaming (rpc @(Protobuf Greeter "sayHelloStreamReply")) name $
      clientLogMsg
