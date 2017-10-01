module Exercise2 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Lecture2

-- Exercise 2 - Joana Correia Magalhães Sousa
generateRandomSet:: Int -> IO (Set Int)
generateRandomSet n = do
                      xs <- getIntL 100 n
                      return (list2set xs)


generateListSets::Int -> IO [(Set Int)]
generateListSets 0 = return []
generateListSets x = do
        y <- getRandomInt 100
        f <- generateRandomSet y
        fs <- generateListSets (x-1)
        return (f:fs)


instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do x <- arbitrary
                 return (list2set x)


main = generateListSets 100
main2 = sample (arbitrary::Gen(Set Int))


--To run the generators, write 'main' and/or 'main2'
--Time spent: 6 hours
--Most of the time spent on this exercise was to try to understand how
--to work with quickcheck generator
