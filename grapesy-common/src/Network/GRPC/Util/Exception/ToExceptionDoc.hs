{-# LANGUAGE CPP               #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Util.Exception.ToExceptionDoc (
    ToExceptionDoc(..)
  , LinesToExceptionDoc(..)
    -- * Top-level rendering functions
  , renderKnown
  , renderAnyException
#if MIN_VERSION_base(4,20,0)
  , renderAnyExceptionAnnotation
#endif
    -- * Interaction with the 'FormatCtx'
  , insertFormatCtx_
  , defaultFormatCtx
  ) where

import Control.Exception (Exception)
import Control.Exception qualified as Base
import Data.Function
import Data.Proxy
import Data.Typeable
import GHC.Generics
import GHC.Stack
import GHC.TypeLits

#if MIN_VERSION_base(4,20,0)
import Control.Exception.Annotation
import Control.Exception.Context
#endif

import Network.GRPC.Util.Exception.Doc
import Network.GRPC.Util.Exception.Exact
import Network.GRPC.Util.Exception.FormatCtx
import Network.GRPC.Util.Exception.Shims

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | 'ToExceptionDoc' is a convenience class
class ToExceptionDoc a where
  toExceptionDoc :: FormatCtx -> a -> Doc

  default toExceptionDoc :: (Generic a, GToDoc (Rep a)) => FormatCtx -> a -> Doc
  toExceptionDoc d = gToDoc d . from

-- | Deriving-via support for 'ToExceptionDoc'
newtype LinesToExceptionDoc a = LinesToExceptionDoc a

instance Show a => ToExceptionDoc (LinesToExceptionDoc a) where
  toExceptionDoc _ (LinesToExceptionDoc x) = fromLines (show x)

{-------------------------------------------------------------------------------
  Top-level rendering functions
-------------------------------------------------------------------------------}

-- | Render known exception or exception annotation
--
-- By default there are two ways to render an exception: 'show' gives us a
-- Haskell value (or is supposed to), perhaps useful to copy/paste into a
-- regression test, and 'displayException' gives us a user-friendly string.
-- Neither of these is particularly useful for developers: 'show' is often
-- unreadable, and 'displayException' may omit information (such as backtraces).
-- We therefore introduce a third way to render an exception, with the
-- additional benefit that we will see all nested exceptions (see also blog post
-- "Exception Annotations: Lay of the Land",
-- <https://well-typed.com/blog/2026/05/lay-annotation-land/>).
renderKnown ::
     ToExceptionDoc e
  => FormatCtx -> e -> String
renderKnown ctx = renderDoc . toExceptionDoc ctx

-- | Render exception of arbitrary type (see also 'renderKnown')
--
-- Unlike 'renderKnown', this does /not/ depend on 'ToExceptionDoc'. Instead,
-- the 'FormatCtx' argument allows user to add ways to print the exceptions they
-- are interested about. 'Exception's being 'Typeable' allows us to not rely on
-- static / type-class mechanisms which in turn allows us to not depend on all
-- downstream packages "too early".
renderAnyException ::
     Exception e
  => FormatCtx -> e -> String
renderAnyException ctx = renderDoc . formatException ctx

#if MIN_VERSION_base(4,20,0)
-- | Render exception annotation of arbitrary type (see also 'renderKnown')
--
-- See 'renderAnyException' for detailed discussion.
renderAnyExceptionAnnotation ::
     ExceptionAnnotation ann
  => FormatCtx -> ann -> String
renderAnyExceptionAnnotation ctx = renderDoc . formatExceptionAnnotation ctx
#endif

{-------------------------------------------------------------------------------
  "Container"-like instances
-------------------------------------------------------------------------------}

instance ToExceptionDoc a => ToExceptionDoc (Maybe a) where
  toExceptionDoc ctx = foldMap (toExceptionDoc ctx)

instance ToExceptionDoc a => ToExceptionDoc [a] where
  toExceptionDoc ctx = foldMap (toExceptionDoc ctx)

{-------------------------------------------------------------------------------
  Generics for 'ToDoc'
-------------------------------------------------------------------------------}

class GToDoc p where
  gToDoc :: FormatCtx -> p a -> Doc

instance ( GToDoc p
         , KnownSymbol typ
         , KnownSymbol modl
         ) => GToDoc (D1 ('MetaData typ modl pkg isNewtype) p) where
  gToDoc ctx (M1 x) =
      withHeader (symbolVal (Proxy @modl) ++ "." ++ symbolVal (Proxy @typ)) $
        gToDoc ctx x

instance ( GToDoc p
         , KnownSymbol constr
         ) => GToDoc (C1 ('MetaCons constr fixity hasFields) p) where
  gToDoc ctx (M1 x) =
      withHeader (symbolVal (Proxy @constr)) $
        gToDoc ctx x

instance ( GToDoc p
         , KnownSymbol fieldSel
         ) => GToDoc (S1 ('MetaSel (Just fieldSel) unpack strict lazy) p) where
  gToDoc ctx (M1 x) =
      withHeader (symbolVal (Proxy @fieldSel)) $
        gToDoc ctx x

instance GToDoc p => GToDoc (S1 ('MetaSel Nothing unpack strict lazy) p) where
  gToDoc ctx (M1 x) = gToDoc ctx x

instance (GToDoc f, GToDoc g) => GToDoc (f :*: g) where
  gToDoc ctx (x :*: y) = gToDoc ctx x <> gToDoc ctx y

instance (GToDoc f, GToDoc g) => GToDoc (f :+: g) where
  gToDoc ctx (L1 x) = gToDoc ctx x
  gToDoc ctx (R1 x) = gToDoc ctx x

instance GToDoc U1 where
  gToDoc _ U1 = mempty

instance ToExceptionDoc a => GToDoc (K1 r a) where
  gToDoc ctx (K1 x) = toExceptionDoc ctx x

{-------------------------------------------------------------------------------
  Instances for common exception /annotations/
-------------------------------------------------------------------------------}

instance ToExceptionDoc CallStack where
  toExceptionDoc _ cs =
      fromLines $ prettyCallStack cs

instance ToExceptionDoc Backtraces where
  toExceptionDoc _ bt =
      fromLines $ displayBacktraces bt

#if MIN_VERSION_base(4,20,0)
instance ToExceptionDoc ExceptionContext where
  toExceptionDoc ctx (ExceptionContext anns) = toExceptionDoc ctx anns
#endif

#ifdef PATCHED_GHC_FOR_EXCEPTION_DEBUGGING
instance ToExceptionDoc Base.WhileHandling where
  toExceptionDoc ctx (Base.WhileHandling cs e) =
      withHeader "WhileHandling" $ mconcat [
          fromLines $ prettyCallStack cs
        , toExceptionDoc ctx e
        ]
#elif MIN_VERSION_base(4,21,0)
instance ToExceptionDoc Base.WhileHandling where
  toExceptionDoc ctx (Base.WhileHandling e) =
      withHeader "WhileHandling" $ toExceptionDoc ctx e
#endif

#if MIN_VERSION_base(4,20,0)
instance ToExceptionDoc SomeExceptionAnnotation where
  toExceptionDoc ctx (SomeExceptionAnnotation ann) =
      formatExceptionAnnotation ctx ann
#endif

#ifndef PATCHED_GHC_FOR_EXCEPTION_DEBUGGING
deriving anyclass instance ToExceptionDoc AtomicallyBacktrace
#endif

{-------------------------------------------------------------------------------
  Instances for common /exceptions/

  NOTE: It is important that for every instance we provide here we also provide
  an entry in the 'defaultFormatCtx'.
-------------------------------------------------------------------------------}

instance ToExceptionDoc Base.SomeException where
  toExceptionDoc ctx (Base.SomeException e) =
      mconcat [
          formatException ctx e
#if MIN_VERSION_base(4,20,0)
        , toExceptionDoc ctx ?exceptionContext
#endif
        ]

instance ToExceptionDoc Base.SomeAsyncException where
  toExceptionDoc ctx (Base.SomeAsyncException e) =
      withHeader "SomeAsyncException" $ formatException ctx e

deriving newtype instance ToExceptionDoc ExactException

{-------------------------------------------------------------------------------
  Interaction with the 'FormatCtx'
-------------------------------------------------------------------------------}

insertFormatCtx_ :: forall e.
     (Typeable e, ToExceptionDoc e)
  => Proxy e -> FormatCtx -> FormatCtx
insertFormatCtx_ _ = insertFormatCtx (toExceptionDoc @e)

-- | Default 'FormatCtx'
--
-- Notes:
--
-- * If we have an exception or exception annotation of known type, and that
--   type has a 'ToExceptionDoc' instance, we can call 'renderKnown'.
--
-- * If we are dealing with exceptions or annotations of unknown type, perhaps
--   defined in other libraries, there /might/ be a 'ToExceptionDoc' instance;
--   but we're not aware of it! This is the purpose of 'renderAnyException' and
--   'renderAnyExceptionAnnotation': instead of doing a static lookup, we accept
--   a 'FormatCtx' as argument which upstream code might have populated with
--   suitable renderers.
--
--   The true upstream solution to this would be to make something add a
--   'ToExceptionDoc' (or similiar) constraint to 'SomeException'l this would
--   obsolete the need for 'FormatCtx'.
--
-- * Since 'renderAnyException' and 'renderAnyExceptionAnnotation' do not have
--   any information to work with other than an 'Exception' or
--   'ExceptionAnnotation' instance, they cannot even take advantage of any
--   instances that we provide here. Therefore it is very important that any
--   'ToExceptionDoc' instance we define gets an entry in this list; without it,
--   if we catch say 'SomeException' somewhere, and call 'renderAnyException' on
--   it, even if it happens to be one of our own types, and that type has a
--   'ToExceptionDoc' instance, we'd still not be able to take advantage of it.
--
-- * The entries for 'SomeException', 'ExactException' and
--   'SomeExceptionAnnotation' are mostly for convenience: /if/ a user calls
--   'renderAnyException' on an argument of type 'SomeException', we'll still get
--   the correct result (the user could call 'renderKnown' instead of course).
defaultFormatCtx :: FormatCtx
defaultFormatCtx = emptyFormatCtx

  --
  -- Exception annotations
  --

  & insertFormatCtx_ (Proxy @CallStack)
  & insertFormatCtx_ (Proxy @Backtraces)
#if MIN_VERSION_base(4,20,0)
  & insertFormatCtx_ (Proxy @ExceptionContext)
  & insertFormatCtx_ (Proxy @SomeExceptionAnnotation)
#endif
#if MIN_VERSION_base(4,21,0)
  & insertFormatCtx_ (Proxy @Base.WhileHandling)
#endif
#ifndef PATCHED_GHC_FOR_EXCEPTION_DEBUGGING
  & insertFormatCtx_ (Proxy @AtomicallyBacktrace)
#endif

  --
  -- Exceptions
  --

  & insertFormatCtx_ (Proxy @Base.SomeException)
  & insertFormatCtx_ (Proxy @Base.SomeAsyncException)
  & insertFormatCtx_ (Proxy @ExactException)
