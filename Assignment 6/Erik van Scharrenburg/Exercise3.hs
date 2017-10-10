module Exercise3 where
import Data.List
import System.Random

-- Exercise 3
-- Unsorted and contains duplicates
composites :: [Integer]
composites = [x * y | x <- [2..], y <- [2..x]]

-- Time: 1 hour
