module Exercise4 where

import Data.List
import System.Random
import Lecture5

-- Exercise 4
genEx4Problem minimal n
  | minimal   = do [r] <- rsolveNs [emptyN]
                   tryUntilMinimal r
  | otherwise = do [r] <- rsolveNs [emptyN]
                   return (eraseBlocks r (take 3 blockPs))
  where tryUntilMinimal r
          | null (unique3Empty r) = genEx4Problem minimal n
          | otherwise = return (head (unique3Empty r))
        unique3Empty r = filter uniqueSol
                           (map (eraseBlocks r) (possibleEraseBlocks n))

showEx4Problem minimal n = do x <- genEx4Problem minimal n
                              showNode x

possibleEraseBlocks :: Int -> [[(Row, Column)]]
possibleEraseBlocks n = nub (map (take n) (permutations blockPs))

eraseBlocks :: Node -> [(Row,Column)] -> Node
eraseBlocks = foldl eraseBlock

eraseBlock :: Node -> (Row,Column) -> Node
eraseBlock n p = foldl eraseN n (block p)

block :: (Row,Column) -> [(Row,Column)]
block (r,c) = [(y,x) | y <- [r..(r+2)], x <- [c..(c+2)]]

blockPs :: [(Row,Column)]
blockPs = [(y,x) | y <- [1,4,7], x <- [1,4,7]]

-- With the definition of a Sudoku problem from the lecture it is possible to
-- create Sudoku problems with up to eight blocks empty.
-- If the Sudoku problem has to be minimal then at least four is possible.
-- This can be checked by running: 'showEx4Problem True 4'
-- Result:
-- +-------+-------+-------+
-- | 1 5 3 | 6 9 2 |       |
-- | 8 4 9 | 5 7 3 |       |
-- | 7 2 6 | 1 8 4 |       |
-- +-------+-------+-------+
-- |       |       | 8 9 2 |
-- |       |       | 3 7 5 |
-- |       |       | 6 1 4 |
-- +-------+-------+-------+
-- | 3 6 7 | 2 5 8 |       |
-- | 2 1 4 | 9 6 7 |       |
-- | 9 8 5 | 3 4 1 |       |
-- +-------+-------+-------+
-- With an implementation that is not random and a lot of time and processing
-- power it should be possible to show that there are no minimal Sudoku problems
-- with five empty blocks by calculating all possibilities and showing that
-- there is more than one solution for each of those.

-- Time: 1 hour and 45 minutes
