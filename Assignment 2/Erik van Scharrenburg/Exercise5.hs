module Exercise5 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Exercise4

-- Recognizing and generating derangements
-- Exercise 5
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = False
isDerangement xs ys = isPermutation xs ys
                   && isDerangement' xs ys
  where isDerangement' [] [] = True
        isDerangement' (x:xs) (y:ys)
          | x /= y    = isDerangement' xs ys
          | otherwise = False
        isDerangement' _ _ = False

deran :: (Eq p, Num p, Enum p) => p -> [[p]]
deran n = filter (isDerangement xs) (permutations xs)
  where xs = [0..n-1]

prop_samelength xs ys = length xs == length ys
prop_allxsinys  xs ys = all (`elem` ys) xs
prop_ordered    xs ys = xs == sort ys
prop_reversed   xs ys = xs == reverse ys
prop_firstperm  xs ys = xs == head (permutations ys)
prop_anyperm    xs ys = xs `elem` permutations ys
prop_equal      xs ys = xs == ys
prop_headequal  xs ys = head xs == head ys

isDerangementTest = forAll isDerangementTestListGen idt
  where idt (xs, ys) = Just (isDerangement xs ys)
                    == lookup (xs, ys) isDerangementTestList

isDerangementTestListGen :: Gen ([Integer], [Integer])
isDerangementTestListGen = elements (map fst isDerangementTestList)
isDerangementTestList :: [(([Integer], [Integer]), Bool)]
isDerangementTestList = [(([], []), False),
                         (([1,2,3], [3,2,1]), False),
                         (([2^65,2,3], [3,2^65,2]), True),
                         (([1], []), False),
                         (([2,4,6], [2,4,6,8]), False),
                         (([1,2,3,7], [1,2]), False),
                         (([1,2,3,7], [2,3,7,1]), True)]

-- 1. Firstperm is stronger than anyperm, samelength, ordered and reversed.
-- 2. Anyperm is stronger than samelength, ordered and reversed.
-- Equal is stronger than headequal.
-- The other properties can not be ordered by strength because none of those is
-- a subset of another property.

-- The test can be automated by running 'quickCheck isDerangementTest'

-- Time: 30 minutes
