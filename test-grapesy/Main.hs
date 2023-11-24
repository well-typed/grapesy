{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import System.Exit
import Text.Show.Pretty

import Network.GRPC.Common
import Network.GRPC.Server (ClientDisconnected(..))

import Test.Driver.ClientServer
import Test.Driver.Dialogue

regression :: IO ()
regression = do
    mResult :: Either SomeException String <- try $
      testClientServer assessCustomException $
       execGlobalSteps globalSteps
    case mResult of
      Left err -> do
        print err
        exitWith $ ExitFailure 1
      Right result -> do
        putStrLn result
        exitWith $ ExitSuccess
  where
    globalSteps :: GlobalSteps
    globalSteps = GlobalSteps [
        LocalSteps [
            ( TestClockTick 1, ClientAction $ Initiate ( Set.fromList [] , RPC1 ))
          , ( TestClockTick 3, ClientAction $ Send (NoMoreElems NoMetadata))
          , ( TestClockTick 5, ServerAction $ Send (NoMoreElems (Set.fromList [])))
          ]
      , LocalSteps [
            ( TestClockTick 0, ClientAction $ Initiate ( Set.fromList [] , RPC1 ))
          , ( TestClockTick 2, ClientAction $ Terminate (Just (ExceptionId 0)))
          , ( TestClockTick 4, ServerAction $ Send (NoMoreElems (Set.fromList [])))
          ]
      ]

data ExpectedUserException =
    ExpectedClientException SomeClientException
  | ExpectedServerException SomeServerException
  | ExpectedForwardedToClient GrpcException
  | ExpectedClientDisconnected SomeException
  | ExpectedEarlyTermination
  deriving stock (Show, GHC.Generic)
  deriving anyclass (PrettyVal)

-- | Custom exceptions
--
-- Technically we should only be expected custom user exceptions when we
-- generate them, but we're not concerned about accidental throws of these
-- custom exceptions.
--
-- TODO: However, it might be useful to be more precise about exactly which
-- gRPC exceptions we expect and when.
assessCustomException :: SomeException -> CustomException ExpectedUserException
assessCustomException err
    --
    -- Custom exceptions
    --

    | Just (userEx :: SomeServerException) <- fromException err
    = CustomExceptionExpected $ ExpectedServerException userEx

    | Just (userEx :: SomeClientException) <- fromException err
    = CustomExceptionExpected $ ExpectedClientException userEx

    -- Server-side exceptions are thrown as 'GrpcException' client-side
    | Just (grpc :: GrpcException) <- fromException err
    , GrpcUnknown <- grpcError grpc
    , Just msg <- grpcErrorMessage grpc
    , "SomeServerException" `Text.isInfixOf` msg
    = CustomExceptionExpected $ ExpectedForwardedToClient grpc

    -- Client-side exceptions are reported as 'ClientDisconnected', but without
    -- additional information (gRPC does not support client-to-server trailers
    -- so we have no way of informing the server about what went wrong).
    | Just (ClientDisconnected e) <- fromException err
    = CustomExceptionExpected $ ExpectedClientDisconnected e

    --
    -- Early termination
    --

    | Just (ChannelDiscarded _) <- fromException err
    = CustomExceptionExpected $ ExpectedEarlyTermination
    | Just (grpc :: GrpcException) <- fromException err
    , GrpcUnknown <- grpcError grpc
    , Just msg <- grpcErrorMessage grpc
    , "ChannelDiscarded" `Text.isInfixOf` msg
    = CustomExceptionExpected $ ExpectedForwardedToClient grpc

    --
    -- Custom wrappers
    --

    | Just (AnnotatedServerException err' _ _) <- fromException err
    = CustomExceptionNested err'

    --
    -- Catch-all
    --

    | otherwise
    = CustomExceptionUnexpected

main :: IO ()
main = regression