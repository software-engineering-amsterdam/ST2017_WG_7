module Exercise3 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise2

-- Exercise 3 - Joana Correia Magalhães Sousa

setIntersection:: Ord a => Set a -> Set a -> Set a
setIntersection (Set []) set2 = Set []
setIntersection (Set (x:xs)) set2
        | inSet x set2  = insertSet x (setIntersection (Set xs) set2)
        | otherwise = setIntersection (Set xs) set2


setDifference:: Ord a => Set a -> Set a -> Set a
setDifference (Set []) set2 = Set []
setDifference (Set (x:xs)) set2
        | not(inSet x set2)  = insertSet x (setDifference (Set xs) set2)
        | otherwise = setDifference (Set xs) set2


setUnion:: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

--------------------------Properties-----------------------------------
--Each element of the result Set must belong in both given sets
propertyInter:: Ord a => Set a -> Set a -> Bool
propertyInter s1 s2 = (subSet result s1) && (subSet result s2)
                      where result = setIntersection s1 s2

--The result set must contain all elements from both given sets
propertyUnion:: Ord a => Set a -> Set a -> Bool
propertyUnion s1 s2 = (subSet s1 result) && (subSet s2 result)
                      where result = setUnion s1 s2

--The result set must contain all elements from the first given set
--that does not belong to the second given set
propertyDiff:: Ord a => Set a -> Set a -> Bool
propertyDiff s1 s2 = if (result == (Set [])) then True else(subSet result s1) && (not(subSet result s2))
                      where result = setDifference s1 s2

-------------------------Testing---------------------------------------

tester::[(Set Int)] -> [(Set Int)]-> ((Set Int) -> (Set Int) -> Bool) -> IO ()
tester [] [] _ = putStrLn "All tested"
tester (f:fs) (t:ts) p = do
        tester fs ts p
        if p f t then putStrLn "Test passed"
        else putStrLn ("Test failed: " ++ show f ++ " " ++ show t)


testSets:: ((Set Int) -> (Set Int) -> Bool) -> IO ()
testSets f = do
        ys <- generateListSets 100
        zs <- generateListSets 100
        (tester ys zs f)

{--

*Lab4_Ex3> testSets propertyInter
All tested
Test passed
Test passed
...
Test passed
*Lab4_Ex3>
(all tests passed)

*Lab4_Ex3> testSets propertyUnion
All tested
Test passed
Test passed
...
Test passed
*Lab4_Ex3>
(all tests passed)

*Lab4_Ex3> testSets propertyDiff
All tested
Test passed
Test passed
...
Test passed
*Lab4_Ex3>
(all tests passed)


*Lab4_Ex3> quickCheck propertyInter
+++ OK, passed 100 tests.
*Lab4_Ex3>
*Lab4_Ex3> quickCheck propertyUnion
+++ OK, passed 100 tests.
*Lab4_Ex3>
*Lab4_Ex3> quickCheck propertyDiff
+++ OK, passed 100 tests.
*Lab4_Ex3>
--}

--Time spent: 3 hours
