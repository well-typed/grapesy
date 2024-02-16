module Test.Driver.Dialogue.TestClock (
    -- * Test clock
    TestClock -- opaque
  , TestClockTick(..)
  , newTestClock
  , waitForTestClockTick
  , advanceTestClock
  , advanceTestClockAtTime
  , advanceTestClockAtTimes
    -- * Interleavings
  , interleave
  , assignTimings
  ) where

import Prelude hiding (id)

import Control.Exception
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import GHC.Generics qualified as GHC
import GHC.Stack
import Test.QuickCheck

import Debug.Concurrent

{-------------------------------------------------------------------------------
  Test clock

  These are concurrent tests, looking at the behaviour of @n@ concurrent
  connections between a client and a server. To make the interleaving of actions
  easier to see, we introduce a global "test clock". Every test action is
  annotated with a particular "test tick" on this clock. The clock is advanced
  after each step: this is a logical clock, unrelated to the passing of time.
-------------------------------------------------------------------------------}

newtype TestClock = TestClock (TVar TestClockTick)

newtype TestClockTick = TestClockTick Int
  deriving stock (Show, GHC.Generic)
  deriving newtype (Eq, Ord, Enum)

data TestClockException = TestClockException SomeException CallStack
  deriving stock (Show)
  deriving anyclass (Exception)

newTestClock :: IO TestClock
newTestClock = TestClock <$> newTVarIO (TestClockTick 0)

-- | Wait for the specified clock tick
--
-- Returns @True@ if we reached the specified tick, or @False@ if the clock is
-- already past the specified tick.
waitForTestClockTick :: HasCallStack => TestClock -> TestClockTick -> IO Bool
waitForTestClockTick (TestClock clock) tick = atomically $
    waitForTick `catchSTM` \err ->
      throwSTM $ TestClockException err callStack
  where
    waitForTick :: STM Bool
    waitForTick = do
      currentTick <- readTVar clock
      if | currentTick >  tick -> return False -- clock already past
         | currentTick == tick -> return True  -- reached specified tick
         | otherwise           -> retry        -- time not yet reached

advanceTestClock :: TestClock -> IO ()
advanceTestClock (TestClock clock) = atomically (modifyTVar clock succ)

advanceTestClockAtTime :: TestClock -> TestClockTick -> IO ()
advanceTestClockAtTime clock tick = do
    reachedTick <- waitForTestClockTick clock tick
    when reachedTick $ advanceTestClock clock

advanceTestClockAtTimes :: TestClock -> [TestClockTick] -> IO ()
advanceTestClockAtTimes = mapM_ . advanceTestClockAtTime

{-------------------------------------------------------------------------------
  Interleavings

  We assign timings /from/ interleavings. This is more suitable to shrinking;
  during generation we pick an arbitrary interleaving, then as we shrink, we
  leave the interleaving (mostly) alone, but re-assign timings.
-------------------------------------------------------------------------------}

-- | Pick an arbitrary interleaving
--
-- This flattens the list; each item in the result is an element from /one/
-- of the input lists, annotated with the index of that particular list.
--
-- > ghci> sample (interleave ["abc", "de"])
-- > [(0,'a'),(1,'d'),(0,'b'),(0,'c'),(1,'e')]
-- > [(1,'d'),(0,'a'),(0,'b'),(1,'e'),(0,'c')]
-- > [(1,'d'),(1,'e'),(0,'a'),(0,'b'),(0,'c')]
interleave :: forall a. [[a]] -> Gen [(Int, a)]
interleave =
    go . mapMaybe (\(i, xs) -> (i,) <$> NE.nonEmpty xs) . zip [0..]
  where
    go :: [(Int, NonEmpty a)] -> Gen [(Int, a)]
    go [] = return []
    go xs = do
        (as, (i, b :| bs), cs) <- isolate xs
        fmap ((i, b) :) $
          case bs of
            []       -> go (as ++ cs)
            b' : bs' -> go (as ++ [(i, b' :| bs')] ++ cs)

-- | Assign timings, given an interleaving
--
-- In some sense this is an inverse to 'interleave':
--
-- >    assignTimings [(0,'a'),(0,'b'),(1,'d'),(1,'e'),(0,'c')]
-- > == [ [ (TestClockTick 0,'a')
-- >      , (TestClockTick 2,'b')
-- >      , (TestClockTick 8,'c')
-- >      ]
-- >    , [ (TestClockTick 4,'d')
-- >      ,( TestClockTick 6,'e')
-- >      ]
-- >    ]
--
-- Put another way
--
-- > map (map snd) . assignTimings <$> interleave xs
--
-- will just generate @xs@.
assignTimings :: forall id a. Ord id => [(id, a)] -> [[(TestClockTick, a)]]
assignTimings = separate . assignClockTicks
  where
    separate :: [(id, x)] -> [[x]]
    separate = go Map.empty
      where
        go :: Map id [x] -> [(id, x)] -> [[x]]
        go acc []             = map reverse $ Map.elems acc
        go acc ((id, x) : xs) = go (Map.alter (Just . maybe [x] (x:)) id acc) xs

    assignClockTicks :: [(id, a)] -> [(id, (TestClockTick, a))]
    assignClockTicks = go (TestClockTick 0)
      where
        go :: TestClockTick -> [(id, a)] -> [(id, (TestClockTick, a))]
        go _ []           = []
        go t ((id, a):as) = (id, (t, a)) : go (succ t) as

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

isolate :: [a] -> Gen ([a], a, [a])
isolate = \case
    [] -> error "isolate: empty list"
    xs -> do n <- choose (0, length xs - 1)
             return $ go [] xs n
  where
    go :: [a] -> [a] -> Int -> ([a], a, [a])
    go _    []     _ = error "isolate: impossible"
    go prev (x:xs) 0 = (reverse prev, x, xs)
    go prev (x:xs) n = go (x:prev) xs (pred n)

