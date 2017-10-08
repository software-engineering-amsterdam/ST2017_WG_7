module Exercise7 where

import Data.List
import System.Random
import qualified Exercise1
import qualified Lecture5

-- Exercise 7
avgHintsSTD :: IO Int
avgHintsSTD = do xs <- numHints 100 (Lecture5.genRandomSudoku,
                                     Lecture5.genProblem,
                                     Lecture5.filledPositions)
                 return (average xs)

avgHintsNRC :: IO Int
avgHintsNRC = do xs <- numHints 100 (Exercise1.genRandomSudoku,
                                     Exercise1.genProblem,
                                     Exercise1.filledPositions)
                 return (average xs)

numHints :: (Eq t1, Num t1, Monad m, Foldable t2)
            => t1 -> (m t3, t3 -> m (t4, b), t4 -> t2 a) -> m [Int]
numHints 0 _ = return []
numHints i (a,b,c) = do s  <- a
                        p  <- b s
                        xs <- numHints (i - 1) (a,b,c)
                        return (length (c (fst p)) : xs)

average :: Foldable t => t Int -> Int
average xs = sum xs `div` length xs

-- The average number of hints in a minimal standard Sudoku problem is
-- approximately 24
-- The average number of hints in a minimal NRC Sudoku problem is approximately
-- 16
-- The number of generated Sudoku problems can be increased and then the result
-- will be more accurate.

-- Time: 30 minutes
