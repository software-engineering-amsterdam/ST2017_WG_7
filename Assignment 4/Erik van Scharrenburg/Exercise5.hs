module Exercise5 where

import Data.List
import System.Random
import Test.QuickCheck

type Rel a = [(a, a)]

-- Exercise 5
symClos :: Ord a => Rel a -> Rel a
symClos a = nubSort (a `union` invRel a)

invRel :: Rel a -> Rel a
invRel = map swap

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

nubSort :: Ord a => [a] -> [a]
nubSort = nub . sort

-- Time: 1 hour
