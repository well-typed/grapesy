{-# LANGUAGE OverloadedStrings #-}

module Test.Driver.Dialogue.Definition (
    -- * Local
    LocalStep(..)
  , Action(..)
  , ClientAction
  , ServerAction
  , RPC(..)
  , TestMetadata(..)
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
import Control.Monad.State (StateT, execStateT, modify)
import Data.Bifunctor
import Data.ByteString qualified as Strict (ByteString)

import Network.GRPC.Common

import Test.Driver.Dialogue.TestClock qualified as TestClock
import Control.Monad.Catch
import GHC.Show (appPrec1, showCommaSpace)

{-------------------------------------------------------------------------------
  Single RPC
-------------------------------------------------------------------------------}

data LocalStep =
    ClientAction ClientAction
  | ServerAction ServerAction
  deriving stock (Show, Eq)

type ClientAction = Action (TestMetadata, RPC) NoMetadata   SomeClientException
type ServerAction = Action TestMetadata        TestMetadata SomeServerException

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

{-------------------------------------------------------------------------------
  Metadata
-------------------------------------------------------------------------------}

data TestMetadata = TestMetadata {
      metadataAsc1 :: Maybe Strict.ByteString
    , metadataAsc2 :: Maybe Strict.ByteString
    , metadataBin3 :: Maybe Strict.ByteString
    , metadataBin4 :: Maybe Strict.ByteString
    }
  deriving (Eq)

-- | Hand-written 'Show' instance which shows @def :: TestMetadata@ as @def@
--
-- This is by far the most common value that shows up in test failures, so this
-- improves readability.
instance Show TestMetadata where
  showsPrec _ (TestMetadata Nothing Nothing Nothing Nothing) = showString "def"
  showsPrec p (TestMetadata asc1 asc2 bin3 bin4) = showParen (p >= appPrec1) $
        showString "TestMetadata {"
      . showString "metadataAsc1 = "
      . showVal asc1
      . showCommaSpace
      . showString "metadataAsc2 = "
      . showVal asc2
      . showCommaSpace
      . showString "metadataBin3 = "
      . showVal bin3
      . showCommaSpace
      . showString "metadataBin4 = "
      . showVal bin4
      . showString "}"
    where
      showVal Nothing  = showString "def"
      showVal (Just x) = showsPrec 0 (Just x)

instance Default TestMetadata where
  def = TestMetadata {
        metadataAsc1 = Nothing
      , metadataAsc2 = Nothing
      , metadataBin3 = Nothing
      , metadataBin4 = Nothing
      }

instance BuildMetadata TestMetadata where
  buildMetadata md = concat [
        [ CustomMetadata "md1"     x | Just x <- [metadataAsc1 md]]
      , [ CustomMetadata "md2"     x | Just x <- [metadataAsc2 md]]
      , [ CustomMetadata "md3-bin" x | Just x <- [metadataBin3 md]]
      , [ CustomMetadata "md4-bin" x | Just x <- [metadataBin4 md]]
      ]

instance ParseMetadata TestMetadata where
  parseMetadata = flip execStateT def . mapM go
    where
      go :: MonadThrow m => CustomMetadata -> StateT TestMetadata m ()
      go md
        | customMetadataName md == "md1"
        = modify $ \x -> x{metadataAsc1 = Just $ customMetadataValue md}

        | customMetadataName md == "md2"
        = modify $ \x -> x{metadataAsc2 = Just $ customMetadataValue md}

        | customMetadataName md == "md3-bin"
        = modify $ \x -> x{metadataBin3 = Just $ customMetadataValue md}

        | customMetadataName md == "md4-bin"
        = modify $ \x -> x{metadataBin4 = Just $ customMetadataValue md}

        | otherwise
        = throwM $ UnexpectedMetadata [md]

instance StaticMetadata TestMetadata where
  metadataHeaderNames _ = ["md1", "md2", "md3-bin", "md4-bin"]

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

