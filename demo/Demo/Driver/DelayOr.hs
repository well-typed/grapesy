module Demo.Driver.DelayOr (
    DelayOr(..)
  , execAll
  , yieldAll
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Tracer
import Pipes

import Network.GRPC.Client (IsFinal(..))

import Demo.Driver.Logging

data DelayOr a =
    Delay Double -- ^ Delay in seconds
  | Exec a       -- ^ Execute the specified RPC
  deriving (Show)

execAll :: forall a. Show a => [DelayOr a] -> IO (IO (IsFinal, a))
execAll = fmap (flip modifyMVar getNext) . newMVar
  where
    getNext :: [DelayOr a] -> IO ([DelayOr a], (IsFinal, a))
    getNext [] =
        error "execAll: empty list"
    getNext [Exec x] = do
        traceWith threadSafeTracer $ "Sending final " ++ show x
        return ([], (Final, x))
    getNext [Delay _] =
        error "execAll: unexpected delay after final message"
    getNext (Exec x:xs) = do
        traceWith threadSafeTracer $ "Sending " ++ show x
        return (xs, (NotFinal, x))
    getNext (Delay d:xs) = do
        traceWith threadSafeTracer $ "Delay " ++ show d ++ "s"
        threadDelay (round (d * 1_000_000))
        traceWith threadSafeTracer $ ""
        getNext xs

yieldAll :: forall a m.
     (MonadIO m, Show a)
  => [DelayOr a] -> Producer' (IsFinal, a) m ()
yieldAll = go
  where
    go :: [DelayOr a] -> Producer' (IsFinal, a) m ()
    go [] =
        error "yieldAll: empty list"
    go [Exec x] = do
        liftIO $ traceWith threadSafeTracer $ "Yielding final " ++ show x
        yield (Final, x)
    go [Delay _] =
        error "yieldAll: unexpected delay after final message"
    go (Exec x:xs) = do
        liftIO $ traceWith threadSafeTracer $ "Yielding " ++ show x
        yield (NotFinal, x)
        go xs
    go (Delay d:xs) = do
        liftIO $ do
          traceWith threadSafeTracer $ "Delay " ++ show d ++ "s"
          threadDelay (round (d * 1_000_000))
          traceWith threadSafeTracer $ ""
        go xs
