module Exercise3 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise2

-- Exercise 3
-- Implementations
intersectS :: Eq a => Set a -> Set a -> Set a
intersectS (Set xs) (Set ys) = Set [x | x <- xs, x `elem` ys]

unionS :: Ord a => Set a -> Set a -> Set a
unionS (Set xs) (Set ys) = Set (unionS' xs ys)
  where unionS' [] [] = []
        unionS' [] ys = ys
        unionS' xs [] = xs
        unionS' xs@(x:xrs) ys@(y:yrs)
          | x < y     = x : unionS' xrs ys
          | x == y    = x : unionS' xrs yrs
          | otherwise = y : unionS' xs  yrs

differenceS :: Eq a => Set a -> Set a -> Set a
differenceS (Set xs) (Set ys) = Set [x | x <- xs, x `notElem` ys]

-- Properties
prop_sameAsBuiltInIntersect :: Set Int -> Set Int -> Bool
prop_sameAsBuiltInIntersect xs ys = intersectS xs ys == intersectS' xs ys
  where intersectS' (Set xs) (Set ys) = Set (nubSort (xs `intersect` ys))

prop_sameAsBuiltInUnion :: Set Int -> Set Int -> Bool
prop_sameAsBuiltInUnion xs ys = unionS xs ys == unionS' xs ys
  where unionS' (Set xs) (Set ys) = Set (nubSort (xs `union` ys))

prop_sameAsBuiltInDifference :: Set Int -> Set Int -> Bool
prop_sameAsBuiltInDifference xs ys = differenceS xs ys == differenceS' xs ys
  where differenceS' (Set xs) (Set ys) = Set (nubSort (xs \\ ys))

-- Tests
testR :: Int -> Int -> (Set Int -> Set Int -> Bool) -> IO ()
testR i n p
  | i == n    = print ("Passed " ++ show n ++ " tests")
  | otherwise = do xs <- genIntSet
                   ys <- genIntSet
                   if p xs ys then
                     testR (i + 1) n p
                   else
                     error ("Error on " ++ show xs ++ " and " ++ show ys)

testIntersectS  = testR 0 100 prop_sameAsBuiltInIntersect
testUnionS      = testR 0 100 prop_sameAsBuiltInUnion
testDifferenceS = testR 0 100 prop_sameAsBuiltInDifference

testQCintersectS  = quickCheck prop_sameAsBuiltInIntersect
testQCunionS      = quickCheck prop_sameAsBuiltInUnion
testQCdifferenceS = quickCheck prop_sameAsBuiltInDifference

-- The implementations pass both the tests with my own generator and the tests
-- using QuickCheck.

-- Time: 1 hour
