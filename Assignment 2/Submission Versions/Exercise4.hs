module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

--Recognizing Permutations
-- Exercise 4 - Joana Correia Magalhães Sousa

isPermutation:: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs ys = elem xs (permutations ys)

--A permutation is a permutation of itself
property1::Eq a => [a] -> Bool
property1 xs = isPermutation xs xs

--A permutation of a list as the same length as the original
property2::Eq a => [a] -> Bool
property2 xs = length xs == length (head (permutations xs))


testR4 :: Int -> Int -> ([Int] -> Bool) -> IO ()
testR4 k n f = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList
                  if (f xs) then
                    do print ("pass on: " ++ show xs)
                       testR4 (k+1) n f
                  else error ("failed test on: " ++ show xs)


--Time spent: 3 hours
