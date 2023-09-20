{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.PrettyVal (
    -- * Deriving-via support
    ShowAsPretty(..)
    -- * CallStack
  , GHC.HasCallStack
  , PrettyCallStack -- opaque
  , prettyCallStack
  ) where

import Control.Exception
import Data.Bifunctor
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void
import GHC.Stack qualified as GHC
import Network.TLS (TLSException)
import Text.Show.Pretty

{-------------------------------------------------------------------------------
  Deriving-via support
-------------------------------------------------------------------------------}

newtype ShowAsPretty a = ShowAsPretty a

instance PrettyVal a => Show (ShowAsPretty a) where
  show (ShowAsPretty x) = dumpStr x

{-------------------------------------------------------------------------------
  CallStack
-------------------------------------------------------------------------------}

newtype PrettyCallStack = PrettyCallStack GHC.CallStack
  deriving Show via ShowAsPretty PrettyCallStack

newtype PrettySrcLoc = PrettySrcLoc GHC.SrcLoc
  deriving Show via ShowAsPretty PrettySrcLoc

instance PrettyVal PrettySrcLoc where
  prettyVal (PrettySrcLoc x) = String (GHC.prettySrcLoc x)

instance PrettyVal PrettyCallStack where
  prettyVal (PrettyCallStack x) =
      prettyVal $ map (second PrettySrcLoc) (GHC.getCallStack x)

prettyCallStack :: GHC.HasCallStack => PrettyCallStack
prettyCallStack = PrettyCallStack GHC.callStack

{-------------------------------------------------------------------------------
  Orphans

  These live only in the testsuite, not the main lib.
-------------------------------------------------------------------------------}

instance PrettyVal () where
  prettyVal () = Con "()" []

instance PrettyVal a => PrettyVal (Set a) where
  prettyVal xs = Con "Set.fromList" [prettyVal $ Set.toList xs]

instance PrettyVal SomeException where
  prettyVal = String . show

instance PrettyVal TLSException where
  prettyVal = String . show

instance PrettyVal Void where
  prettyVal = absurd