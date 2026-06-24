{-# LANGUAGE CPP               #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Exception utilities
--
-- Most users will never need to import from this module; it's mostly here to
-- facilitate debugging, primarily of @grapesy@ itself and the libraries it
-- depends on, such as @http2@.
module Network.GRPC.Common.Exception (
    -- * Exception rendering
    renderException
  , formatException
  , FormatCtx
  , defaultFormatCtx
  , insertFormatCtx
  , LinesToExceptionDoc(..)
  , ToExceptionDoc(..)
  , renderDoc
  , Doc
  , withHeader
  , fromLines

    -- * Util
  , catchAndWrap

    -- * ExactException
  , ExactException(..)
  , throwExact
  , withoutAnnotations

    -- ** Catching 'ExactException'
    --
    -- This is primarily useful to avoid accidentally throwing 'SomeException'.
  , catchExact
  , tryExact
  , waitCatchExact

    -- * Shims

    -- ** Throwing
  , throwIO
  , throwM

    -- ** Backtraces
  , Backtraces
  , collectBacktraces
  , displayBacktraces

    -- ** Annotations
  , WithAnnotations
  , pattern WithAnnotations
  , annotateIO
  , addExceptionContext

    -- ** STM
#ifdef PATCHED_GHC_FOR_EXCEPTION_DEBUGGING
  , STM.atomically
#else
  , AtomicallyBacktrace(..)
  , atomically
#endif
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Exception (SomeException(..))
import Control.Exception qualified as Base
import Control.Monad.Catch qualified as Exceptions
import Data.Bifunctor
import Data.Foldable qualified as Foldable
import Data.Semigroup
import Data.String
import Data.Typeable
import GHC.Generics
import GHC.Stack
import GHC.TypeLits
import System.ThreadManager qualified as TimeManager
import Type.Reflection qualified as Reflection

#if MIN_VERSION_base(4,20,0)
import Control.Exception.Annotation
import Control.Exception.Backtrace qualified as Backtrace
import Control.Exception.Context
import Control.Exception (backtraceDesired)
#endif

import Network.GRPC.Util.Imports

{-------------------------------------------------------------------------------
  Internal auxiliary: barebones rendering abstraction for nested indentation
-------------------------------------------------------------------------------}

data Doc = FromString String | VCat [Doc] | Indent Int Doc

instance IsString Doc where
  fromString = FromString

instance Monoid Doc where
  mempty  = VCat []
  mconcat = VCat

instance Semigroup Doc where
  a <> b  = VCat [a, b]
  sconcat = VCat . Foldable.toList

fromLines :: String -> Doc
fromLines = mconcat . map fromString . lines

renderDoc :: Doc -> String
renderDoc = \d ->
      unlines
    $ map (\(i, str) -> replicate i ' ' ++ str)
    $ go [(0, d)]
  where
    go :: [(Int, Doc)] -> [(Int, String)]
    go []            = []
    go ((i, d) : ds) =
        case d of
          FromString str   -> (i, str) : go ds
          VCat    ds'   -> go $ map (i,) ds' ++ ds
          Indent     i' d' -> go $ (i + i', d') : ds

withHeader :: String -> Doc -> Doc
withHeader header body = mconcat [
      fromString header
    , Indent 2 body
    ]

{-------------------------------------------------------------------------------
  Construct readable exception rendering
-------------------------------------------------------------------------------}

-- | Render exception for the benefit of a developer
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
--
-- The 'FormatCtx' argument allows user to add ways to print the exceptions
-- they are interested about. 'Exception's being 'Typeable' allows us to not rely
-- on static / type-class mechanisms which in turn allows us to not depend on
-- all downstream packages "too early".
renderException :: Exception e => FormatCtx -> e -> String
renderException d e = renderDoc (formatException d e)

-- | 'ToExceptionDoc' is a convenience class
class ToExceptionDoc a where
  toExceptionDoc :: FormatCtx -> a -> Doc

  default toExceptionDoc :: (Generic a, GToDoc (Rep a)) => FormatCtx -> a -> Doc
  toExceptionDoc d = gToDoc d . from

newtype FormatCtx = FormatCtx [Renderer] -- we would like to use dependent-map but it doesn't support GHC-9.14

data Renderer where
    Renderer :: Reflection.TypeRep e -> (FormatCtx -> e -> Doc) -> Renderer

defaultFormatCtx :: FormatCtx
defaultFormatCtx = FormatCtx []
  & insertFormatCtx_ (Proxy @Base.SomeAsyncException)
  & insertFormatCtx_ (Proxy @TimeManager.KilledByThreadManager)

insertFormatCtx :: Typeable e => (FormatCtx -> e -> Doc) -> FormatCtx -> FormatCtx
insertFormatCtx f (FormatCtx xs) = FormatCtx (Renderer Reflection.typeRep f : xs)

insertFormatCtx_ :: forall e. (Typeable e, ToExceptionDoc e) => Proxy e -> FormatCtx -> FormatCtx
insertFormatCtx_ _ = insertFormatCtx (toExceptionDoc @e)

lookupFormatCtx :: Reflection.TypeRep e -> FormatCtx -> Maybe (FormatCtx -> e -> Doc)
lookupFormatCtx ty (FormatCtx xs) = go xs where
    go [] = Nothing
    go (Renderer ty' f : xs') = case Reflection.eqTypeRep ty ty' of
        Just HRefl -> Just f
        Nothing    -> go xs'

formatException :: Exception e => FormatCtx -> e -> Doc
formatException ctx e = case lookupFormatCtx ty ctx of
    Just f  -> f ctx e
    Nothing -> withHeader (show ty) $ fromLines $ displayException e
  where
    ty = Reflection.typeOf e

-- | Deriving-via support for 'ToExceptionDoc'
newtype LinesToExceptionDoc a = LinesToExceptionDoc a

instance Show a => ToExceptionDoc (LinesToExceptionDoc a) where
  toExceptionDoc _ (LinesToExceptionDoc x) = fromLines (show x)

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
  Instances for specific types
-------------------------------------------------------------------------------}

instance ToExceptionDoc a => ToExceptionDoc (Maybe a) where
  toExceptionDoc ctx = foldMap (toExceptionDoc ctx)

instance ToExceptionDoc a => ToExceptionDoc [a] where
  toExceptionDoc ctx = foldMap (toExceptionDoc ctx)

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

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

-- | Wrap an exception
--
-- Notes:
--
-- * Since the original exception is wrapped as-is, including any annotations,
--   we use 'catchExact' to avoid adding a 'WhileHandling' annotation.
-- * We use 'throwIO' to throw the new wrapped exception, so that /if/ the
--   exception wrapper has 'backtraceDesired', we get a backtrace to the wrap.
catchAndWrap ::
     (HasCallStack, Exception e)
  => (ExactException -> e) -> IO a -> IO a
catchAndWrap f io =
    io `catchExact` (throwIO . f)
#if !MIN_VERSION_base(4,20,0)
  where
    _suppressWarning :: CallStack
    _suppressWarning = callStack
#endif

{-------------------------------------------------------------------------------
  'ExactException'
-------------------------------------------------------------------------------}

-- | Exception with emphasis on accurate annotations
--
-- When this type appears in the @grapesy@ API, it emphasises that we have tried
-- to ensure that any exception annotations are taken seriously.
--
-- Unlike the 'Exception' instance for 'SomeException', the instance for
-- 'ExactException' can be used with 'throwIO', 'throwTo', 'cancelWith', etc.,
-- without losing any annotations.
--
-- See also 'catchExact', 'tryExact', 'waitCatchExact'.
newtype ExactException = WrapExactException {
      unwrapExactException :: SomeException
    }
  deriving stock (Show)
  deriving newtype (ToExceptionDoc)

instance Exception ExactException where
  fromException    = Just . WrapExactException
  toException      = unwrapExactException
  displayException = displayException . unwrapExactException
#if MIN_VERSION_base(4,20,0)
  backtraceDesired = const False
#endif

-- | Type-specialized wrapper around throwIO, to avoid mistakes
--
-- This does not need a `HasCallSTack` constraint, because no backtrace is
-- added to `ExactException`.
throwExact :: ExactException -> IO a
throwExact = throwIO

withoutAnnotations :: ExactException -> (forall e. Exception e => e -> r) -> r
withoutAnnotations (WrapExactException (SomeException e)) k = k e

{-------------------------------------------------------------------------------
  Catching 'ExactException'

  This is primarily useful to avoid accidentally throwing 'SomeException'.
-------------------------------------------------------------------------------}

-- | Catch 'ExactException'
--
-- Won't install any other kind of exception handler (i.e., no 'WhileHandling'
-- annotation will be added). This is comparable to 'catchNoPropagate' (see
-- discussion in 'ExactException'); there is no analogue of 'rethrowIO',
-- since 'throwIO' /itself/ can be used safely with 'ExactException'.
catchExact :: IO a -> (ExactException -> IO a) -> IO a
#if !MIN_VERSION_base(4,21,0)
catchExact = Base.catch
#else
catchExact action handler =
    Base.catchNoPropagate action (handler . aux)
  where
    -- NOTE: only used in GHC 9.12 and up
    aux :: Base.ExceptionWithContext SomeException -> ExactException
    aux (Base.ExceptionWithContext _ctxt se) = WrapExactException se
#endif

tryExact :: IO a -> IO (Either ExactException a)
tryExact = Base.try

waitCatchExact :: Async a -> STM (Either ExactException a)
waitCatchExact = fmap (first WrapExactException) . waitCatchSTM

{-------------------------------------------------------------------------------
  Shim: throwing
-------------------------------------------------------------------------------}

throwIO :: (Exception e, HasCallStack) => e -> IO a
throwIO = Base.throwIO
#if !MIN_VERSION_base(4,20,0)
  where
    _suppressRedundantConstraintWarning = callStack
#endif

throwM :: (Exception e, Exceptions.MonadThrow m, HasCallStack) => e -> m a
throwM = Exceptions.throwM
#if !MIN_VERSION_exceptions(0,10,6)
  where
    _suppressRedundantConstraintWarning = callStack
#endif

{-------------------------------------------------------------------------------
  Shim: bug-free version of 'ExceptionWithContext'

  In GHC 9.10 'ExceptionWithContext' is broken (throwing something of type
  @ExceptionWithContext SomeException@ will result in nested @SomeException@,
  breaking exception handlers). Prior to GHC 9.10 it is not available at all.
  The implementation here stays as close as possible to the one in GHC 9.14.
-------------------------------------------------------------------------------}

#if !MIN_VERSION_base(4,20,0)

data ExceptionContext = EmptyExceptionContext
  deriving stock (Show)

data WithAnnotations a = WithAnnotations ExceptionContext a
  deriving stock (Show)

instance Exception e => Exception (WithAnnotations e) where
  toException (WithAnnotations EmptyExceptionContext e) =
      toException e

  fromException se = do
      e <- fromException se
      return (WithAnnotations EmptyExceptionContext e)

  displayException = displayException . toException

#elif !MIN_VERSION_base(4,21,0)

-- | Bug-free replacement for 'ExceptionWithContext' in GHC 9.10
data WithAnnotations e = WithAnnotations ExceptionContext e
  deriving stock Generic
  deriving anyclass ToExceptionDoc

instance Exception a => Show (WithAnnotations a) where
  show (WithAnnotations _ctxt e) = show e

instance Exception a => Exception (WithAnnotations a) where
    toException (WithAnnotations ctxt e) =
        case toException e of
          SomeException c ->
            let ?exceptionContext = ctxt
            in SomeException c
    fromException se = do
        e <- fromException se
        return (WithAnnotations (Base.someExceptionContext se) e)
    backtraceDesired (WithAnnotations _ e) = backtraceDesired e
    displayException = displayException . toException

#else

type WithAnnotations = Base.ExceptionWithContext

pattern WithAnnotations :: ExceptionContext -> a -> WithAnnotations a
pattern WithAnnotations ctxt e = Base.ExceptionWithContext ctxt e

#endif

{-------------------------------------------------------------------------------
  Shim: Show-able wrapper for 'Backtraces' (or 'CallStack' for GHC < 9.10)
-----------------------------------------------------------------------------}

#if !MIN_VERSION_base(4,20,0)
newtype Backtraces = WrapBacktraces {
      unwrapBacktraces :: CallStack
    }
  deriving stock (Show)

collectBacktraces :: HasCallStack => IO Backtraces
collectBacktraces = return $ WrapBacktraces GHC.Stack.callStack

displayBacktraces :: Backtraces -> String
displayBacktraces = prettyCallStack . unwrapBacktraces

#else

newtype Backtraces = WrapBacktraces {
      unwrapBacktraces :: Backtrace.Backtraces
    }

-- Frustratingly, 'Backtraces' does not have a law-abiding 'Show' instance
instance Show Backtraces where
  show = displayBacktraces

collectBacktraces :: HasCallStack => IO Backtraces
collectBacktraces = WrapBacktraces <$> Backtrace.collectBacktraces

displayBacktraces :: Backtraces -> String
displayBacktraces = Backtrace.displayBacktraces . unwrapBacktraces

#endif

{-------------------------------------------------------------------------------
  Shim: Make `annotateIO` a no-op for GHC < 9.10
-------------------------------------------------------------------------------}

#if !MIN_VERSION_base(4,20,0)

annotateIO :: ann -> IO a -> IO a
annotateIO _ = id

addExceptionContext :: ann -> SomeException -> SomeException
addExceptionContext _ = id

#else

annotateIO ::
     ExceptionAnnotation ann
  => ann -> IO a -> IO a
annotateIO = Base.annotateIO

addExceptionContext ::
     ExceptionAnnotation ann
  => ann -> SomeException -> SomeException
addExceptionContext = Base.addExceptionContext

#endif

{-------------------------------------------------------------------------------
  Special-casing: exception annotations
-------------------------------------------------------------------------------}

instance ToExceptionDoc SomeException where
  toExceptionDoc ctx (SomeException e) =
      mconcat [
          formatException ctx e
#if MIN_VERSION_base(4,20,0)
        , toExceptionDoc ctx ?exceptionContext
#endif
        ]

instance ToExceptionDoc Base.SomeAsyncException where
  toExceptionDoc ctx (Base.SomeAsyncException e) =
      withHeader "SomeAsyncException" $ formatException ctx e

instance ToExceptionDoc TimeManager.KilledByThreadManager where
  toExceptionDoc ctx = \case
      TimeManager.KilledByThreadManager mse ->
        withHeader "KilledByThreadManager" $ toExceptionDoc ctx mse

{- TODO: add to the http2 specific tests.
instance ToExceptionDoc HTTP2.HTTP2Error where

-}

{-------------------------------------------------------------------------------
  Special-casing: exceptions
-------------------------------------------------------------------------------}

#if MIN_VERSION_base(4,20,0)

data SpecialCaseAnnotation =
    CaseBacktraces Backtraces
#if MIN_VERSION_base(4,21,0)
  | CaseWhileHandling Base.WhileHandling
#endif

specialCaseAnnotation :: Typeable ann => ann -> Maybe SpecialCaseAnnotation
specialCaseAnnotation ann = asum [
      CaseBacktraces    <$> cast ann
#if MIN_VERSION_base(4,21,0)
    , CaseWhileHandling <$> cast ann
#endif
    ]

instance ToExceptionDoc SomeExceptionAnnotation where
  toExceptionDoc ctx (SomeExceptionAnnotation ann) =
      maybe (renderGenericAnnotation ann) (toExceptionDoc ctx) $
        specialCaseAnnotation ann

-- | Render generic annotation (no special-casing)
renderGenericAnnotation :: ExceptionAnnotation ann => ann -> Doc
renderGenericAnnotation = fromLines . displayExceptionAnnotation

instance ToExceptionDoc SpecialCaseAnnotation where
  toExceptionDoc ctx = \case
      CaseBacktraces    x -> toExceptionDoc ctx x
#if MIN_VERSION_base(4,21,0)
      CaseWhileHandling x -> toExceptionDoc ctx x
#endif

#endif

{-------------------------------------------------------------------------------
  STM support
-------------------------------------------------------------------------------}

#ifdef PATCHED_GHC_FOR_EXCEPTION_DEBUGGING
-- Nothing to do, part of the patch
#else
-- | Backtrace to a call to 'atomically'
--
-- When an STM transaction throws an exception, this will tell us where that
-- tranaction was invoked (though not where /within/ the transaction it
-- threw an exception).
newtype AtomicallyBacktrace = AtomicallyBacktrace Backtraces
  deriving stock (Generic)
  deriving anyclass (ToExceptionDoc)

#if MIN_VERSION_base(4,20,0)
instance ExceptionAnnotation AtomicallyBacktrace where
  displayExceptionAnnotation = renderDoc . toExceptionDoc defaultFormatCtx
#endif

atomically :: HasCallStack => STM a -> IO a
atomically stm = do
    backtraces <- collectBacktraces
    annotateIO (AtomicallyBacktrace backtraces) $
      STM.atomically stm
#endif
