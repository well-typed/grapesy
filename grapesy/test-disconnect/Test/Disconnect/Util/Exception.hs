{-# LANGUAGE CPP #-}

module Test.Disconnect.Util.Exception (
    testFormatCtx
  , uncaughtExceptionHandler
  ) where

import Control.Concurrent
import Control.Exception
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import System.IO

#if MIN_VERSION_base(4,18,0)
import GHC.Conc.Sync (threadLabel)
#endif

import Network.HTTP2.Client qualified as HTTP2

import Network.GRPC.Common.Exception

{-------------------------------------------------------------------------------
  Exception rendering
-------------------------------------------------------------------------------}

testFormatCtx :: FormatCtx
testFormatCtx = grapesyFormatCtx
    & insertFormatCtx http2
  where
    http2 :: FormatCtx -> HTTP2.HTTP2Error -> Doc
    http2 ctx = \case
      HTTP2.BadThingHappen se ->
        withHeader "BadThingHappen" $ toExceptionDoc ctx se
      other ->
        fromLines $ displayException other

{-------------------------------------------------------------------------------
  Uncaught exception handler
-------------------------------------------------------------------------------}

uncaughtExceptionHandler :: SomeException -> IO ()
uncaughtExceptionHandler e = do
    tid    <- myThreadId
    mLabel :: Maybe String <-
#if MIN_VERSION_base(4,18,0)
      threadLabel tid
#else
      return $ Just "unknown label"
#endif
    hPutStrLn stderr $ concat [
         "Uncaught exception in "
      , show tid
      , " ("
      , fromMaybe "unlabelled" mLabel
      , "): "
      , renderAnyException testFormatCtx e
      ]
