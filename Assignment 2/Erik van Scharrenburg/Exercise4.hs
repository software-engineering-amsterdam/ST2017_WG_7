module Exercise4 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Recognizing Permutations
-- Exercise 4
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = ip (Just xs) (Just ys)
  where ip (Just [])     (Just []) = True
        ip (Just (x:xs)) (Just ys) = ip (Just xs) (removeFirst x ys)
        ip _             _         = False

removeFirst :: Eq a => a -> [a] -> Maybe [a]
removeFirst x xs = rf x (xs, [])
  where rf _ ([], _) = Nothing
        rf r (x:xs, ys)
          | r == x = Just (reverse ys ++ xs)
          | otherwise = rf r (xs, x : ys)

-- Testable properties:
isTrue _ _ = True
prop_samelength xs ys = length xs == length ys
prop_allxsinys  xs ys = all (`elem` ys) xs
prop_ordered    xs ys = xs == sort ys
prop_reversed   xs ys = xs == reverse ys
prop_firstperm  xs ys = xs == head (permutations ys)
prop_anyperm    xs ys = xs `elem` permutations ys
prop_equal      xs ys = xs == ys
prop_headequal  xs ys = head xs == head ys
-- Test with lists:
isPermutationTest = forAll isPermutationTestListGen ipt
  where ipt (xs, ys) = Just (isPermutation xs ys)
                    == lookup (xs, ys) isPermutationTestList
isPermutationTestListGen :: Gen ([Integer], [Integer])
isPermutationTestListGen = elements (map fst isPermutationTestList)
isPermutationTestList :: [(([Integer], [Integer]), Bool)]
isPermutationTestList = [(([], []), True),
                         (([1,2,3], [3,2,1]), True),
                         (([2^65,-2,3], [3,2^65,-2]), True),
                         (([1], []), False),
                         (([2,4,6], [2,4,6,8]), False),
                         (([1,2,3,7], [1,2]), False)]
-- The assumption that the input lists do not contain duplicates simplifies the
-- testing procedure because the specification is stricter.
--
-- 1. Firstperm is stronger than anyperm, samelength, ordered and reversed.
-- 2. Anyperm is stronger than samelength, ordered and reversed.
-- Equal is stronger than headequal.
-- The other properties can not be ordered by strength because none of those is
-- a subset of another property.

-- Hoare triples:
-- { prop_samelength xs ys && prop_allxsinys xs ys } res = isPermutation xs ys
--   { res }
-- { prop_ordered xs ys } res = isPermutation xs ys { res }
-- { prop_reversed xs ys } res = isPermutation xs ys { res }
-- { prop_firstperm xs ys } res = isPermutation xs ys { res }
-- { prop_anyperm xs ys } res = isPermutation xs ys { res }
-- { isTrue xs ys } res = isPermutation xs ys { res == prop_anyperm xs ys }
-- The last triple has the weakest precondition and the strongest
-- postcondition possible thus the strongest testing procedure is:
strongTest xs ys = isTrue xs ys
               ==> let res = isPermutation xs ys in
                     res == prop_anyperm xs ys
-- This test will be very slow for large lists but limiting the size of the
-- lists will weaken the test procedure.
-- The tests can be automated with QuickCheck by running
-- 'quickCheck strongTest' or 'quickCheck isPermutationTest'

-- Time: 5 hours
