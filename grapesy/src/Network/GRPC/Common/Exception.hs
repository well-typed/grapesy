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
    ToExceptionDoc -- opaque
  , renderException
  , LinesToExceptionDoc(..)

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

    -- * Shim: backtraces
  , Backtraces
  , collectBacktraces
  , displayBacktraces

    -- * Shim: WithAnnotations (ExceptionWithContext)
  , WithAnnotations
  , pattern WithAnnotations
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Data.Bifunctor
import Data.Foldable qualified as Foldable
import Data.Semigroup
import Data.String
import Data.Typeable
import GHC.Generics
import GHC.Stack
import GHC.TypeLits

#if MIN_VERSION_base(4,20,0)
import Control.Exception.Annotation
import Control.Exception.Backtrace qualified as Backtrace
import Control.Exception.Context
#endif

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
class ToExceptionDoc a where
  toExceptionDoc :: a -> Doc

  default toExceptionDoc :: (Generic a, GToDoc (Rep a)) => a -> Doc
  toExceptionDoc = gToDoc . from

renderException :: ToExceptionDoc e => e -> String
renderException = renderDoc . toExceptionDoc

newtype LinesToExceptionDoc a = LinesToExceptionDoc a

instance Show a => ToExceptionDoc (LinesToExceptionDoc a) where
  toExceptionDoc (LinesToExceptionDoc x) = fromLines (show x)

{-------------------------------------------------------------------------------
  Generics for 'ToDoc'
-------------------------------------------------------------------------------}

class GToDoc p where
  gToDoc :: p a -> Doc

instance ( GToDoc p
         , KnownSymbol typ
         , KnownSymbol modl
         ) => GToDoc (D1 ('MetaData typ modl pkg isNewtype) p) where
  gToDoc (M1 x) = mconcat [
        fromString $ concat [
            symbolVal (Proxy @modl)
          , "."
          , symbolVal (Proxy @typ)
          ]
      , Indent 2 $ gToDoc x
      ]

instance ( GToDoc p
         , KnownSymbol constr
         ) => GToDoc (C1 ('MetaCons constr fixity hasFields) p) where
  gToDoc (M1 x) = mconcat [
        fromString $ symbolVal (Proxy @constr)
      , Indent 2 $ gToDoc x
      ]

instance ( GToDoc p
         , KnownSymbol fieldSel
         ) => GToDoc (S1 ('MetaSel (Just fieldSel) unpack strict lazy) p) where
  gToDoc (M1 x) = mconcat [
        fromString $ symbolVal (Proxy @fieldSel)
      , Indent 2 $ gToDoc x
      ]

instance GToDoc p => GToDoc (S1 ('MetaSel Nothing unpack strict lazy) p) where
  gToDoc (M1 x) = gToDoc x

instance (GToDoc f, GToDoc g) => GToDoc (f :*: g) where
  gToDoc (x :*: y) = gToDoc x <> gToDoc y

instance (GToDoc f, GToDoc g) => GToDoc (f :+: g) where
  gToDoc (L1 x) = gToDoc x
  gToDoc (R1 x) = gToDoc x

instance GToDoc U1 where
  gToDoc U1 = mempty

instance ToExceptionDoc a => GToDoc (K1 r a) where
  gToDoc (K1 x) = toExceptionDoc x

{-------------------------------------------------------------------------------
  Instances for specific types
-------------------------------------------------------------------------------}

instance ToExceptionDoc a => ToExceptionDoc (Maybe a) where
  toExceptionDoc = foldMap toExceptionDoc

instance ToExceptionDoc a => ToExceptionDoc [a] where
  toExceptionDoc = foldMap toExceptionDoc

instance ToExceptionDoc SomeException where
  toExceptionDoc se@(SomeException e) = mconcat [
          fromString $ show (typeOf e)
        , Indent 2 $ mconcat [
              fromLines $ displayException e
            , toExceptionDoc classified
            ]
        ]
    where
      classified :: Classified
      classified = classifyAnnotations se

instance ToExceptionDoc CallStack where
  toExceptionDoc cs =
      fromLines $ prettyCallStack cs

#if MIN_VERSION_base(4,20,0)
instance ToExceptionDoc SomeExceptionAnnotation where
  toExceptionDoc (SomeExceptionAnnotation a) =
      fromLines $ displayExceptionAnnotation a

instance ToExceptionDoc ExceptionContext where
  toExceptionDoc (ExceptionContext anns) = toExceptionDoc anns
#endif

#if MIN_VERSION_base(4,21,0)
instance ToExceptionDoc WhileHandling where
  toExceptionDoc (WhileHandling e) = mconcat [
        "WhileHandling"
      , Indent 2 $ toExceptionDoc e
      ]
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
throwExact :: HasCallStack => ExactException -> IO a
throwExact = throwIO
#if !MIN_VERSION_base(4,20,0)
  where
    _suppressWarning :: CallStack
    _suppressWarning = callStack
#endif

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
catchExact = catch
#else
catchExact action handler =
    catchNoPropagate action (handler . aux)
  where
    -- NOTE: only used in GHC 9.12 and up
    aux :: ExceptionWithContext SomeException -> ExactException
    aux (ExceptionWithContext _ctxt se) = WrapExactException se
#endif

tryExact :: IO a -> IO (Either ExactException a)
tryExact = try

waitCatchExact :: Async a -> STM (Either ExactException a)
waitCatchExact = fmap (first WrapExactException) . waitCatchSTM

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
  deriving stock (Generic)
  deriving anyclass ToExceptionDoc

instance Show a => Show (WithAnnotations a) where
  show (WithAnnotations ctxt e) =
      renderException $ WithAnnotations ctxt (LinesToExceptionDoc e)

instance Exception a => Exception (WithAnnotations a) where
    toException (WithAnnotations ctxt e) =
        case toException e of
          SomeException c ->
            let ?exceptionContext = ctxt
            in SomeException c
    fromException se = do
        e <- fromException se
        return (WithAnnotations (someExceptionContext se) e)
    backtraceDesired (WithAnnotations _ e) = backtraceDesired e
    displayException = displayException . toException

#else

type WithAnnotations = ExceptionWithContext

pattern WithAnnotations :: ExceptionContext -> a -> WithAnnotations a
pattern WithAnnotations ctxt e = ExceptionWithContext ctxt e

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

instance ToExceptionDoc Backtraces where
  toExceptionDoc bt =
      fromLines $ displayBacktraces bt

{-------------------------------------------------------------------------------
  Shim: Make 'WhileHandling' an empty type for GHC < 9.12
-------------------------------------------------------------------------------}

#if !MIN_VERSION_base(4,21,0)

data WhileHandling

instance ToExceptionDoc WhileHandling where
  toExceptionDoc x = case x of {}

#endif

{-------------------------------------------------------------------------------
  Classify exception annotations
-------------------------------------------------------------------------------}

#if !MIN_VERSION_base(4,20,0)

-- No annotations prior to GHC 9.10
data Classified = NoAnnotations

classifyAnnotations :: SomeException -> Classified
classifyAnnotations _ = NoAnnotations

instance ToExceptionDoc Classified where
  toExceptionDoc NoAnnotations = mempty

#else

data Classified = Classified{
      other         :: [SomeExceptionAnnotation]
    , backtraces    :: Maybe Backtraces
    , whileHandling :: Maybe WhileHandling
    }

instance ToExceptionDoc Classified where
  toExceptionDoc Classified{other, backtraces, whileHandling} = mconcat [
        foldMap toExceptionDoc other
      , foldMap toExceptionDoc backtraces
      , foldMap toExceptionDoc whileHandling
      ]

classifyAnnotations :: SomeException -> Classified
classifyAnnotations se =
    let ExceptionContext annotations = someExceptionContext se
    in go Classified{
              other         = []
            , backtraces    = Nothing
            , whileHandling = Nothing
            }
          annotations
  where
    go :: Classified -> [SomeExceptionAnnotation] -> Classified
    go acc [] = acc{other = reverse (other acc)}
    go acc (sa@(SomeExceptionAnnotation a):as)
      | Just a' <- cast a, Nothing <- backtraces acc
      = go acc{backtraces = Just a'} as

      | Just a' <- cast a, Nothing <- whileHandling acc
      = go acc{whileHandling = Just a'} as

      | otherwise
      = go acc{other = sa:other acc} as

#endif

