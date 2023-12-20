module Test.Driver.Dialogue.Definition (
    -- * Local
    LocalStep(..)
  , Action(..)
  , RPC(..)
  , Metadata
    -- * Bird's-eye view
  , GlobalSteps(..)
  , LocalSteps(..)
    -- * Exceptions
    -- ** User exceptions
  , SomeClientException(..)
  , SomeServerException(..)
  , ExceptionId(..)
    -- ** Wrappers
  , AnnotatedServerException(..)
    -- * Utility
  , hasEarlyTermination
  ) where

import Control.Exception
import Data.Set (Set)
import GHC.Generics qualified as GHC
import Text.Show.Pretty

import Network.GRPC.Common
import Network.GRPC.Internal

import Test.Driver.Dialogue.TestClock
import Test.Util.PrettyVal

{-------------------------------------------------------------------------------
  Single channel

  TODO: We should test that the Trailers-Only case gets triggered if no messages
  were exchanged before the exception (not sure this is observable without
  Wireshark..?).

  TODO: Test what happens when either peer simply disappears.
-------------------------------------------------------------------------------}

data LocalStep =
    ClientAction (Action (Metadata, RPC) NoMetadata)
  | ServerAction (Action Metadata        Metadata)
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (PrettyVal)

data Action a b =
    -- | Initiate request and response
    --
    -- When the client initiates a request, they can specify a timeout, initial
    -- metadata for the request, as well as which endpoint to connect to. This
    -- must happen before anything else.
    --
    -- On the server side an explicit 'Initiate' is not required; if not
    -- present, there will be an implicit one, with empty metadata, on the first
    -- 'Send'.
    Initiate a

    -- | Send a message to the peer
  | Send (StreamElem b Int)

    -- | Early termination (cleanly or with an exception)
  | Terminate (Maybe ExceptionId)

    -- | Sleep specified number of milliseconds
    --
    -- This is occassionally useful, for example to have the client keep the
    -- connection open to the server for a bit longer, without actually doing
    -- anything with that connection.
  | SleepMilli Int
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (PrettyVal)

data RPC = RPC1 | RPC2 | RPC3
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (PrettyVal)

-- | Metadata
--
-- We use 'Set' for 'CustomMetadata' rather than a list, because we do not
-- want to test that the /order/ of the metadata is matched.
type Metadata = Set CustomMetadata

{-------------------------------------------------------------------------------
  Many channels (bird's-eye view)
-------------------------------------------------------------------------------}

newtype LocalSteps = LocalSteps {
      getLocalSteps :: [(TestClockTick, LocalStep)]
    }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (PrettyVal)

newtype GlobalSteps = GlobalSteps {
      getGlobalSteps :: [LocalSteps]
    }
  deriving stock (GHC.Generic)
  deriving anyclass (PrettyVal)
  deriving Show via ShowAsPretty GlobalSteps

{-------------------------------------------------------------------------------
  User exceptions

  When a test calls for the client or the server to throw an exception, we throw
  one of these. Their sole purpose is to be "any" kind of exception (not a
  specific one).
-------------------------------------------------------------------------------}

data SomeServerException = SomeServerException ExceptionId
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Exception, PrettyVal)

data SomeClientException = SomeClientException ExceptionId
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Exception, PrettyVal)

-- | We distinguish exceptions from each other simply by a number
newtype ExceptionId = ExceptionId Int
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Exception wrappers
-------------------------------------------------------------------------------}

-- | Annotated server handler exception
--
-- When a server handler throws an exception, it is useful to know what that
-- particular handler was executing.
data AnnotatedServerException = AnnotatedServerException {
       serverGlobalException          :: SomeException
     , serverGlobalExceptionSteps     :: LocalSteps
     , serverGlobalExceptionCallStack :: PrettyCallStack
     }
  deriving stock (GHC.Generic)
  deriving anyclass (PrettyVal)
  deriving Show via ShowAsPretty AnnotatedServerException
  deriving Exception via ExceptionWrapper AnnotatedServerException

instance HasNestedException AnnotatedServerException where
  getNestedException = serverGlobalException

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

hasEarlyTermination :: GlobalSteps -> Bool
hasEarlyTermination =
      any isEarlyTermination
    . map snd
    . concatMap getLocalSteps
    . getGlobalSteps
  where
    isEarlyTermination :: LocalStep -> Bool
    isEarlyTermination (ClientAction (Terminate _)) = True
    isEarlyTermination (ServerAction (Terminate _)) = True
    isEarlyTermination _                            = False

