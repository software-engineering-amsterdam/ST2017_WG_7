module Lab2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Control.Monad
import Control.Monad.Reader

--1
probs :: Int -> IO [Float]
probs 0 = return []

probs n = do
    p <- getStdRandom random
    ps <- probs (n-1) 
    return (p:ps)
--1

--2
quicksort :: Ord a => [a] -> [a]  
quicksort [] = []

quicksort (x:xs) = 
   quicksort [ a | a <- xs, a <= x ]  
   ++ [x]
   ++ quicksort [ a | a <- xs, a > x ]
--2

--listOfLists [] = []

--cumulativeList n = 
    --if n < 0.25 then x1 else
    --if n >= 0.25 && n < 0.5 then x2 else
    --if n >= 0.5 && n < 0.75 then x3 else x4

test_exercise1:: Int -> IO [Float]

test_exercise1 n = do
    myList <- probs n
    firstInterval <- filter (liftM2 (&&) (> 0) (< 0.25)) myList
    return (firstInterval)
    --secondInterval <- filter (liftM2 (&&) (>= 0.25) (< 0.5)) myList
    --thirdInterval <- filter (liftM2 (&&) (>= 0.5) (< 0.75)) myList
    --forthInterval <- filter (liftM2 (&&) (>= 0.75) (< 1)) myList
    --return (quicksort myList)
    --map cumulativeList myList
    --return myList

--1&2 from Lab2