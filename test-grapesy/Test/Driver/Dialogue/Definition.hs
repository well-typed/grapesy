module Test.Driver.Dialogue.Definition (
    -- * Local
    LocalStep(..)
  , Action(..)
  , ClientAction
  , ServerAction
  , RPC(..)
  , Metadata(Metadata)
  , getMetadata
    -- * Bird's-eye view
  , GlobalSteps(..)
  , LocalSteps(..)
    -- * Exceptions
    -- ** User exceptions
  , SomeClientException(..)
  , SomeServerException(..)
  , ExceptionId
    -- * Utility
  , hasEarlyTermination
  ) where

import Control.Exception
import Data.Bifunctor
import Data.Function (on)
import Data.List (sortBy, nubBy)

import Network.GRPC.Common

import Test.Driver.Dialogue.TestClock qualified as TestClock

{-------------------------------------------------------------------------------
  Single RPC
-------------------------------------------------------------------------------}

data LocalStep =
    ClientAction ClientAction
  | ServerAction ServerAction
  deriving stock (Show, Eq)

type ClientAction = Action (Metadata, RPC) NoMetadata SomeClientException
type ServerAction = Action Metadata        Metadata   SomeServerException

data Action a b e =
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
  | Terminate (Maybe e)
  deriving stock (Show, Eq)

data RPC = RPC1 | RPC2 | RPC3
  deriving stock (Show, Eq)

-- | Metadata
--
-- For the sake of these tests, we don't want to deal with duplicate headers
-- (this is tested separately in the serialization tests). We do however ensure
-- that this listed is always ordered (header ordering is not guaranteed).
newtype Metadata = UnsafeMetadata { getMetadata :: [CustomMetadata] }
  deriving stock (Show, Eq)

mkMetadata :: [CustomMetadata] -> Metadata
mkMetadata =
      UnsafeMetadata
    . sortBy (compare `on` customMetadataName)
    . nubBy  ((==)    `on` customMetadataName)

pattern Metadata :: [CustomMetadata] -> Metadata
pattern Metadata mds <- UnsafeMetadata mds
  where
    Metadata = mkMetadata

{-# COMPLETE Metadata #-}

{-------------------------------------------------------------------------------
  Many RPCs (bird's-eye view)
-------------------------------------------------------------------------------}

newtype LocalSteps = LocalSteps {
      getLocalSteps :: [(TestClock.Tick, LocalStep)]
    }
  deriving stock (Show)

newtype GlobalSteps = GlobalSteps {
      getGlobalSteps :: [LocalSteps]
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  User exceptions

  When a test calls for the client or the server to throw an exception, we throw
  one of these. Their sole purpose is to be "any" kind of exception (not a
  specific one).
-------------------------------------------------------------------------------}

data SomeServerException = SomeServerException ExceptionId
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

data SomeClientException = SomeClientException ExceptionId
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

-- | We distinguish exceptions from each other simply by a number
type ExceptionId = Int

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

-- | Check if the client or server terminate early
hasEarlyTermination :: GlobalSteps -> (Bool, Bool)
hasEarlyTermination =
      bimap or or
    . unzip
    . map (isEarlyTermination . snd)
    . concatMap getLocalSteps
    . getGlobalSteps
  where
    isEarlyTermination :: LocalStep -> (Bool, Bool)
    isEarlyTermination (ClientAction (Terminate _)) = (True, False)
    isEarlyTermination (ServerAction (Terminate _)) = (False, True)
    isEarlyTermination _                            = (False, False)

