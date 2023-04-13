module Network.GRPC.Util.List (
    markLast
  ) where

{-------------------------------------------------------------------------------
  Lists
-------------------------------------------------------------------------------}

-- | Mark the last element of the list
--
-- This is useful for example when sending a bunch of messages, and the remote
-- node needs to be told that the last message is the final one.
--
-- >>> markLast [1..5]
-- [(1,False),(2,False),(3,False),(4,False),(5,True)]
--
-- This function is lazy:
--
-- >>> take 5 $ markLast [1..]
-- [(1,False),(2,False),(3,False),(4,False),(5,False)]
markLast :: [a] -> [(a, Bool)]
markLast = \case
    []   -> []
    x:xs -> go x xs
  where
    go :: a -> [a] -> [(a, Bool)]
    go prev []     = [(prev, True)]
    go prev (x:xs) = (prev, False) : go x xs
