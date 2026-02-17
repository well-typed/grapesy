module Test.Driver.Dialogue.TestClock (
    TestClock -- opaque
  , Tick      -- opaque
  , new
    -- * Current time
  , waitForTick
  , advance
    -- * Green light
  , waitForGreenLight
  , giveGreenLight
    -- * Interleavings
  , interleave
  , assignTimings
  ) where

import Prelude hiding (id)

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack
import Test.QuickCheck (Gen, choose)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Test clock
--
-- When we are testing concurrent code, we may get different results depending
-- on the exact way that the various threads are scheduled, potentially making
-- test outcomes difficult to specify. Moreover, even if the final outcome does
-- not depend on the exact scheduling, debugging tests is much simpler if they
-- are deterministic and do the same thing each time. Consider tracing a
-- particular test execution, or inspecting it through Wireshark: if the test
-- execution is different each time, debugging becomes much more difficult.
--
-- We therefore introduce a global \"test clock\", and randomly assign each
-- action to be executed a \"clock tick\" (formally, we are exploring a /random
-- interleaving/). This then begs the question of when the clock gets advanced.
-- We could just have a thread tick the test clock every @n@ milliseconds on the
-- wallclock, but choosing @n@ is hard: too low, and we /still/ execute actions
-- concurrently; too high, and the tests run much slower than they need to.
-- Instead, we say that the actions /themselves/ are responsible for advancing
-- the test clock when appropriate.
--
-- In a concurrent setting, the \"when appropriate\" part is however not always
-- easy to determine. Suppose we have thread A sending messages to thread B,
-- then there is no need to wait for each message to have been /received/ by B
-- (this would reduce the test to effectively using the network layer in a
-- synchronous manner); this would suggest we can tick the clock immediately
-- after sending (rather than waiting for the message to be received first).
-- However, if at some point later A throws a (deliberate) exception, then A
-- /should/ wait for B to have received all messages first, or risk that the
-- exception could \"overtake\" some message, leading to flaky tests.
--
-- Getting this right is surprisingly subtle. A knows to wait for B, but only
-- once it gets to the action that throws the exception; at that point it wants
-- to know that B received the messages it sent /in some previous action/. The
-- approach we take attempts to avoid this \"spooky action at a distance\" as
-- follows: when A is about to throw a deliberate exception at tick T, it waits
-- to get the green light for T; on B's side, when B is ready to /react/ to this
-- exception (implying any previous actions have been verified already), it
-- /gives/ the green light for T. Since A knows when it needs to wait for the
-- green light (e.g., throwing a deliberate exception) and when it doesn't
-- (e.g., sending a message), we avoid over-sychronization. Meanwhile, B giving
-- the green light for an action rather than acknowledging some previous action
-- keeps the tests easier to understand: A and B are both talking about the same
-- tick T.
newtype TestClock = TestClock (TVar State)

-- | State of the clock
data State = State {
      stateNow   :: Tick
    , stateGreen :: Set Tick
    }

-- | Clock tick
newtype Tick = Tick Word
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

-- | Start the clock
new :: IO TestClock
new = TestClock <$> newTVarIO initState
  where
    initState :: State
    initState = State {
        stateNow   = Tick 0
      , stateGreen = Set.empty
      }

{-------------------------------------------------------------------------------
  Current time
-------------------------------------------------------------------------------}

-- | Thrown by 'waitForTick'
data TimePassed = TimePassed {
      timePassedNow    :: Tick
    , timePassedWanted :: Tick
    , timePassedAt     :: CallStack
    }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Wait for specified clock tick
--
-- If the clock has already gone past the specified time, throws 'TimePassed'.
waitForTick :: MonadIO m => TestClock -> Tick -> m ()
waitForTick (TestClock clock) t = liftIO . atomically $ do
    State{stateNow} <- readTVar clock
    if | stateNow < t  -> retry
       | stateNow == t -> return ()
       | otherwise     -> throwSTM $ TimePassed {
                              timePassedNow    = stateNow
                            , timePassedWanted = t
                            , timePassedAt     = callStack
                            }

-- | Advance the clock by one tick
--
-- There should only ever be /one/ thread responsible for advancing the clock
-- at any given time.
advance :: MonadIO m => TestClock -> m ()
advance (TestClock clock) = liftIO . atomically $ do
    modifyTVar clock $ \st@State{stateNow} ->
      st{stateNow = succ stateNow}

{-------------------------------------------------------------------------------
  Green light
-------------------------------------------------------------------------------}

-- | Wait for green light
--
-- See 'TestClock' for discussion.
waitForGreenLight :: MonadIO m => TestClock -> Tick -> m ()
waitForGreenLight (TestClock clock) t = liftIO . atomically $ do
    State{stateGreen} <- readTVar clock
    unless (t `Set.member` stateGreen) retry

-- | Give green light
--
-- See 'TestClock' for discussion.
giveGreenLight :: MonadIO m => TestClock -> Tick -> m ()
giveGreenLight (TestClock clock) t = liftIO . atomically $ do
    modifyTVar clock $ \st@State{stateGreen} ->
      st{stateGreen = Set.insert t stateGreen}

{-------------------------------------------------------------------------------
  Computing interleavings

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
-- > == [ [ (Tick 0,'a')
-- >      , (Tick 2,'b')
-- >      , (Tick 8,'c')
-- >      ]
-- >    , [ (Tick 4,'d')
-- >      ,( Tick 6,'e')
-- >      ]
-- >    ]
--
-- Put another way
--
-- > map (map snd) . assignTimings <$> interleave xs
--
-- will just generate @xs@.
assignTimings :: forall id a. Ord id => [(id, a)] -> [[(Tick, a)]]
assignTimings = separate . assignClockTicks
  where
    separate :: [(id, x)] -> [[x]]
    separate = go Map.empty
      where
        go :: Map id [x] -> [(id, x)] -> [[x]]
        go acc []             = map reverse $ Map.elems acc
        go acc ((id, x) : xs) = go (Map.alter (Just . maybe [x] (x:)) id acc) xs

    assignClockTicks :: [(id, a)] -> [(id, (Tick, a))]
    assignClockTicks = go (Tick 0)
      where
        go :: Tick -> [(id, a)] -> [(id, (Tick, a))]
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
