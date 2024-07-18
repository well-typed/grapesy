-- | Demonstration of using a custom monad stack
module Demo.Client.API.StreamType.MonadStack.Greeter (
    sayHello
  , sayHelloStreamReply
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.CanCallRPC
import Network.GRPC.Common.NextElem qualified as NextElem
import Network.GRPC.Common.Protobuf

import Demo.Common.API
import Demo.Client.Util.Logging qualified as Logging

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

sayHello :: Connection -> Proto HelloRequest -> IO ()
sayHello conn name = runMyClient conn $ do
    reply <- nonStreaming (rpc @SayHello) name
    clientLogMsg reply

sayHelloStreamReply :: Connection -> Proto HelloRequest -> IO ()
sayHelloStreamReply conn name = runMyClient conn $ do
    serverStreaming (rpc @SayHelloStreamReply) name $ \recv ->
      NextElem.whileNext_ (liftIO recv) clientLogMsg
