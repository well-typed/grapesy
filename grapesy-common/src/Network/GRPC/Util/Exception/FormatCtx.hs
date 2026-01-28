{-# LANGUAGE CPP #-}

-- | Custom overrides for exception rendering
module Network.GRPC.Util.Exception.FormatCtx (
    FormatCtx -- opaque
    -- * Construction
  , emptyFormatCtx
  , insertFormatCtx
    -- * Use
  , formatException
#if MIN_VERSION_base(4,20,0)
  , formatExceptionAnnotation
#endif
  ) where

import Control.Exception
import Data.Typeable
import Type.Reflection qualified as Reflection

#if MIN_VERSION_base(4,20,0)
import Control.Exception.Annotation
#endif

import Network.GRPC.Util.Exception.Doc

{-------------------------------------------------------------------------------
  Definition

  TODO: It would be nicer to use dependent-map for 'FormatCtx' but it doesn't
  support GHC-9.14.
-------------------------------------------------------------------------------}

-- | Custom renderers
--
-- This can be used to dynamically dispatch rendering for exception or
-- exception annotations to user-specified renderers, overriding any instances
-- that may (or may not) be in scope.
newtype FormatCtx = FormatCtx [Renderer]

data Renderer where
    Renderer :: Reflection.TypeRep e -> (FormatCtx -> e -> Doc) -> Renderer

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

emptyFormatCtx :: FormatCtx
emptyFormatCtx = FormatCtx []

insertFormatCtx :: Typeable e => (FormatCtx -> e -> Doc) -> FormatCtx -> FormatCtx
insertFormatCtx f (FormatCtx xs) = FormatCtx (Renderer Reflection.typeRep f : xs)

{-------------------------------------------------------------------------------
  Use
-------------------------------------------------------------------------------}

formatException :: Exception e => FormatCtx -> e -> Doc
formatException ctx e = case lookupFormatCtx ty ctx of
    Just f  -> f ctx e
    Nothing -> withHeader (show ty) $ fromLines $ displayException e
  where
    ty = Reflection.typeOf e

#if MIN_VERSION_base(4,20,0)
formatExceptionAnnotation :: ExceptionAnnotation ann => FormatCtx -> ann -> Doc
formatExceptionAnnotation ctx ann = case lookupFormatCtx ty ctx of
    Just f  -> f ctx ann
    Nothing -> withHeader (show ty) $ fromLines $ displayExceptionAnnotation ann
  where
    ty = Reflection.typeOf ann
#endif

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

lookupFormatCtx :: Reflection.TypeRep e -> FormatCtx -> Maybe (FormatCtx -> e -> Doc)
lookupFormatCtx ty (FormatCtx xs) = go xs where
    go [] = Nothing
    go (Renderer ty' f : xs') = case Reflection.eqTypeRep ty ty' of
        Just HRefl -> Just f
        Nothing    -> go xs'
