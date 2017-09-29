{-# LANGUAGE FlexibleInstances #-}
module Exercise2 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- Exercise 2
genIntSet :: IO (Set Int)
genIntSet = do n  <- getStdRandom (randomR (0, 100))
               xs <- randomIntL n
               return (Set (nubSort xs))
  where randomIntL :: Int -> IO [Int]
        randomIntL 0 = return []
        randomIntL n = do x  <- getStdRandom random
                          xs <- randomIntL (n - 1)
                          return (x : xs)

instance Arbitrary (Set Int) where
  arbitrary = genIntSet'

genIntSet' :: Gen (Set Int)
genIntSet' = do xs <- arbitrary
                return (Set (nubSort xs))

nubSort :: Ord a => [a] -> [a]
nubSort = nub . sort

-- Time: 30 minutes
