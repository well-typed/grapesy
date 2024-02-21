module Demo.Client.Util.DelayOr (
    DelayOr(..)
  , execAll
  , yieldAll
  ) where

import Control.Concurrent hiding (yield)
import Control.Tracer
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Pipes

import Network.GRPC.Common

import Demo.Common.Logging

data DelayOr a =
    Delay Double -- ^ Delay in seconds
  | Exec a       -- ^ Execute the specified RPC
  deriving (Show)

isDelay :: DelayOr a -> Either Double a
isDelay (Delay d) = Left d
isDelay (Exec  a) = Right a

execAll :: forall a. Show a => [DelayOr a] -> IO (IO (StreamElem NoMetadata a))
execAll =
    fmap (flip modifyMVar getNext) . newMVar . alternating . map isDelay
  where
    getNext ::
         AltLists Double a
      -> IO (AltLists Double a, (StreamElem NoMetadata a))
    getNext (Alternating Nil) =
        return (Alternating Nil, NoMoreElems NoMetadata)
    getNext (Alternating (Lft ds xss)) = do
        let d = sum ds
        traceWith threadSafeTracer $ "Delay " ++ show d ++ "s"
        threadDelay (round (d * 1_000_000))
        traceWith threadSafeTracer $ ""
        getNext (Alternating xss)
    getNext (Alternating (Rgt (a :| as) xss)) = do
        traceWith threadSafeTracer $ "Sending " ++ show a
        return (
            case as of
              []     -> Alternating xss
              a':as' -> Alternating (Rgt (a' :| as') xss)
          , checkIsFinal (a :| as) xss
          )

yieldAll :: forall a m.
     (MonadIO m, Show a)
  => [DelayOr a] -> Producer' (StreamElem NoMetadata a) m ()
yieldAll = withAlternating go . alternating . map isDelay
  where
    go ::
         Alt d (NonEmpty Double) (NonEmpty a)
      -> Producer' (StreamElem NoMetadata a) m ()
    go Nil =
        yield $ NoMoreElems NoMetadata
    go (Lft ds xss) = do
        let d = sum ds
        liftIO $ do
          traceWith threadSafeTracer $ "Delay " ++ show d ++ "s"
          threadDelay (round (d * 1_000_000))
          traceWith threadSafeTracer $ ""
        go xss
    go (Rgt (a :| as) xss) = do
        liftIO $ traceWith threadSafeTracer $ "Yielding " ++ show a
        yield $ checkIsFinal (a :| as) xss
        case as of
          []     -> go xss
          a':as' -> go (Rgt (a' :| as') xss)

checkIsFinal ::
     NonEmpty a
  -> Alt L (NonEmpty Double) (NonEmpty a)
  -> StreamElem NoMetadata a
checkIsFinal (a :| [])      Nil               = FinalElem  a NoMetadata
checkIsFinal (a :| [])      (Lft _ Nil)       = FinalElem  a NoMetadata
checkIsFinal (a :| [])      (Lft _ (Rgt _ _)) = StreamElem a
checkIsFinal (a :| (_ : _))  _                = StreamElem a

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

data Dir = L | R

data Alt (d :: Dir) a b where
  Nil :: Alt d a b
  Lft :: a -> Alt R a b -> Alt L a b
  Rgt :: b -> Alt L a b -> Alt R a b

data Alternating a b where
  Alternating :: Alt d a b -> Alternating a b

deriving instance (Show a, Show b) => Show (Alt d a b)
deriving instance (Show a, Show b) => Show (Alternating a b)

withAlternating :: (forall d. Alt d a b -> r) -> Alternating a b -> r
withAlternating f (Alternating xs) = f xs

type AltLists a b = Alternating (NonEmpty a) (NonEmpty b)

alternating :: [Either a b] -> AltLists a b
alternating = \case
    []         -> Alternating Nil
    Left  a:xs -> Alternating $ goA (a :| []) xs
    Right b:xs -> Alternating $ goB (b :| []) xs
  where
    goA :: NonEmpty a -> [Either a b] -> Alt L (NonEmpty a) (NonEmpty b)
    goA acc []           = Lft (NE.reverse acc) Nil
    goA acc (Left  a:xs) = goA (NE.cons a acc) xs
    goA acc (Right b:xs) = Lft (NE.reverse acc) (goB (b :| []) xs)

    goB :: NonEmpty b -> [Either a b] -> Alt R (NonEmpty a) (NonEmpty b)
    goB acc []           = Rgt (NE.reverse acc) Nil
    goB acc (Right b:xs) = goB (NE.cons b acc) xs
    goB acc (Left  a:xs) = Rgt (NE.reverse acc) (goA (a :| []) xs)
