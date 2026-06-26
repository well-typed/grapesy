{-# LANGUAGE CPP               #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Exception utilities
--
-- Most users will never need to import from this module; it's mostly here to
-- facilitate debugging, primarily of @grapesy@ itself and the libraries it
-- depends on, such as @http2@.
module Network.GRPC.Common.Exception (
    -- * Key exception types
    ClientDisconnected(..)
  , ServerDisconnected(..)

    -- * Rendering
  , grapesyFormatCtx

    -- * Re-exports
  , module X
  ) where

import Data.Typeable
import GHC.Generics
import System.ThreadManager qualified as TimeManager

#if MIN_VERSION_base(4,20,0)
import Control.Exception (backtraceDesired)
#endif

import Network.GRPC.Util.Imports

import Network.GRPC.Util.Exception.Doc            as X
import Network.GRPC.Util.Exception.Exact          as X
import Network.GRPC.Util.Exception.FormatCtx      as X
import Network.GRPC.Util.Exception.Shims          as X
import Network.GRPC.Util.Exception.ToExceptionDoc as X

{-------------------------------------------------------------------------------
  Key exception types

  TODO: /All/ exceptions that indicate a client disconnect or a server
  disconnect should be wrapped in these two types, so that client code does not
  have to deal with many exception types.
  <https://github.com/well-typed/grapesy/issues/339>
-------------------------------------------------------------------------------}

-- | Client disconnected unexpectedly
data ClientDisconnected = ClientDisconnected {
      -- | The exception as reported by the transport (typically @http2@)
      clientDisconnectedException :: ExactException

      -- | Backtrace, /if/ different
      --
      -- In most cases we wrap the underlying exception the moment it arises.
      -- When this happens, there is no meaningful difference between the
      -- backtrace associated with the exception and the backtrace to where we
      -- wrap it.
      --
      -- Sometimes, however, transport exceptions are thrown asynchronously. For
      -- example, @http2@ will throw 'KilledByThreadManager' to server handlers
      -- when a client disconnects. In this case the backtrace of the exception
      -- and where we catch it might be quite different.
    , clientDisconnectedBacktrace :: Maybe Backtraces
    }
  deriving stock (Generic, Show)
  deriving anyclass ToExceptionDoc

-- | Server disconnected unexpectedly
--
-- See also 'ClientDisconnected' for additional discussion.
data ServerDisconnected = ServerDisconnected {
      -- | The exception as reported by the transport (typically @http2@)
      serverDisconnectedException :: ExactException

      -- | Backtrace, /if/ different
      --
      -- As discussed in 'clientDisconnectedBacktrace', normally we there is
      -- no meaningful difference between the backtrace of the exception and the
      -- backtrace to where we wrap it. An example where they /are/ different is
      -- when the entire connection to the server is closed, and we notice this
      -- elsewhere and close individual RPCs on that connection; the backtrace
      -- of the latter will be different to the former.
    , serverDisconnectedBacktrace :: Maybe Backtraces
    }
  deriving stock (Generic, Show)
  deriving anyclass ToExceptionDoc

#if MIN_VERSION_base(4,20,0)
-- See discussion of 'clientDisconnectedBacktrace'
instance Exception ClientDisconnected where backtraceDesired _ = False
instance Exception ServerDisconnected where backtraceDesired _ = False
#else
instance Exception ClientDisconnected
instance Exception ServerDisconnected
#endif

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

instance ToExceptionDoc TimeManager.KilledByThreadManager where
  toExceptionDoc ctx = \case
      TimeManager.KilledByThreadManager mse ->
        withHeader "KilledByThreadManager" $ toExceptionDoc ctx mse

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

grapesyFormatCtx :: FormatCtx
grapesyFormatCtx = defaultFormatCtx
  & insertFormatCtx_ (Proxy @ClientDisconnected)
  & insertFormatCtx_ (Proxy @ServerDisconnected)
  & insertFormatCtx_ (Proxy @TimeManager.KilledByThreadManager)
