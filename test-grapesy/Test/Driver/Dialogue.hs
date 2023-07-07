module Test.Driver.Dialogue (
    RequestState(..)
  , ResponseState(..)
  , ClientState
  , ServerState
  , Interaction(..)
  , Interactions(..)
  , Dialogue(..)
  , execDialogue
  ) where

import Control.Concurrent
import Data.Default
import Data.Kind
import Data.SOP

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common.CustomMetadata (CustomMetadata)
import Network.GRPC.Server qualified as Server
import Network.GRPC.Common.Binary

import Test.Driver.ClientServer
import Test.Util.SOP

{-------------------------------------------------------------------------------
  Dialogue
-------------------------------------------------------------------------------}

type DialogueRpc = BinaryRpc "binary" "dialogue"

data RequestState =
    RequestStarted
  | RequestEnded

type ClientState = [RequestState]

data RequestData :: RequestState -> Type where
  RequestOpen   :: Client.Call DialogueRpc -> RequestData RequestStarted
  RequestClosed :: RequestData RequestEnded

data ResponseState =
    ResponseStarted
  | ResponseEnded

type ServerState = [ResponseState]

data Interaction :: (ClientState, ServerState) -> (ClientState, ServerState) -> Type where
  StartRequest ::
       Maybe Client.Timeout
    -> [CustomMetadata]
    -> Interaction '(reqs, resps) '(RequestStarted ': reqs, resps)
  CloseRequest ::
       Update RequestData RequestStarted RequestEnded reqs reqs'
    -> Interaction '(reqs, resps) '(reqs', resps)

deriving stock instance Show (Interaction st st')

data Interactions :: (ClientState, ServerState) -> (ClientState, ServerState) -> Type where
  Done :: Interactions '(client0, server0) '(client0, server0)
  Step :: Interaction  '(client0, server0) '(client1, server1)
       -> Interactions '(client1, server1) '(client2, server2)
       -> Interactions '(client0, server0) '(client2, server2)

deriving stock instance Show (Interactions st st')

data Dialogue :: Type where
  Dialogue :: Interactions '( '[] , '[] ) '(reqs , resps) -> Dialogue

deriving instance Show Dialogue

execDialogue :: Dialogue -> ClientServerTest
execDialogue (Dialogue interactions) = def {
      client = \conn ->
        clientSide conn interactions
    , server = do
        interactionsVar <- newMVar interactions
        return [serverSide interactionsVar]
    }
  where
    clientSide ::
         Client.Connection
      -> Interactions '( '[], '[] ) '(reqs, resps)
      -> IO ()
    clientSide conn = go Nil
      where
        go :: NP RequestData reqs -> Interactions '(reqs, a) '(reqs', b) -> IO ()
        go _ Done =
            return ()
        go reqs (Step (StartRequest timeout metadata) is) = do
            req <- aux
            go (req :* reqs) is
          where
            aux :: IO (RequestData 'RequestStarted)
            aux =
                RequestOpen <$>
                  Client.startRPC conn params (Proxy @DialogueRpc)

            params :: Client.CallParams
            params = Client.CallParams {
                  callTimeout         = timeout
                , callRequestMetadata = metadata
                }
        go reqs (Step (CloseRequest ix) is) = do
            reqs' <- updateAtM aux ix reqs
            go reqs' is
          where
            aux :: RequestData 'RequestStarted -> IO (RequestData 'RequestEnded)
            aux (RequestOpen call) = do
                Client.abortRPC call
                return $ RequestClosed

    serverSide ::
         MVar (Interactions '( '[], '[] ) '(reqs, resps))
      -> Server.RpcHandler IO
    serverSide _interactionsVar = undefined {- (Server.mkRpcHandler (Proxy @DialogueRpc) go) {
          Server.handlerMetadata = \_requestMetadata ->
            -- TODO: Hmm this is weird. we don't get to control /when/ the
            -- response is sent back; what if we want the response metadata
            -- to depend on some input /messages/ and not just the input
            -- metadata? This is a problem with the API
            undefined
        }
      where
        go :: Server.Call DialogueRpc -> IO ()
        go _call = undefined

     -}


