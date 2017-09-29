module Lab4_Ex7 where
 
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Lecture2
import Lab4_Ex2
import Lab4_Ex5
import Lab4_Ex6

-----------------------------Properties--------------------------------

--The resulst of symClos must be a union of R and its inverse
propsymClos::(Eq a, Ord a) => Rel a -> Bool
propsymClos rel = propSymClos rel (symClos rel)

propSymClos::(Eq a, Ord a) => Rel a -> Rel a -> Bool
propSymClos [] _ = True
propSymClos ((x,y):xs) ys
        | (elem (x,y) ys) && (elem (y,x) ys) = propSymClos xs ys
        | otherwise = False


--The result of trClos is the smallest relation on set X that contains R and is transitive.
proptrClos::(Eq a, Ord a) => Rel a -> Bool
proptrClos rel = propTrClos rel rel (trClos rel)

propTrClos::(Eq a, Ord a) => Rel a -> Rel a -> Rel a -> Bool
propTrClos _ _ [] = True
propTrClos relOrg rel1 rel2 = if (rel3 == rel2) then False else propTrClos relOrg rel4 rel3
                            where rel3 = filter (\f -> not(elem f rel2)) rel1
                                  rel4 = (relOrg @@ rel1)

--------------------------------Testing--------------------------------
generateRandomRel:: Int -> IO (Rel Int)
generateRandomRel n = do
                      xs <- generateRandomSet n
                      ys <- generateListTuples n xs
                      return $ sort(nub ys)

generateListTuples:: Int -> (Set Int) -> IO [(Int,Int)]
generateListTuples 0 _ = return []
generateListTuples n (Set xs) = do
                          t <- getRandomInt ((length xs)-1)
                          u <- getRandomInt ((length xs)-1)
                          let y = xs !! t
                              z = xs !! u
                          fs <- (generateListTuples (n-1) (Set xs))
                          return  ((y,z):fs)


generateListRels::Int -> IO [(Rel Int)]
generateListRels 0 = return []
generateListRels x = do
        y <- getRandomInt 100
        f <- generateRandomRel y
        fs <- generateListRels (x-1)
        return (f:fs)

tester::[(Rel Int)] -> ((Rel Int) -> Bool) -> IO ()
tester [] _ = putStrLn "All tested"
tester (f:fs) p = do
        tester fs p
        if p f then putStrLn "Test passed"
        else putStrLn ("Test failed: " ++ show f)


testRels:: ((Rel Int) -> Bool) -> IO ()
testRels f = do
        xs <- generateListRels 100 
        (tester xs f)

{--
*Lab4_Ex7> testRels propsymClos
All tested
Test passed
Test passed
...
Test passed
(all tests passed)
*Lab4_Ex7>
*Lab4_Ex7>
*Lab4_Ex7> testRels proptrClos
All tested
Test passed
Test passed
...
Test passed
(all tests passed)
--}

------------------------Testing with QuickCheck------------------------

   
{--
*Lab4_Ex7> quickCheck propsymClos
+++ OK, passed 100 tests
*Lab4_Ex7>
*Lab4_Ex7>
*Lab4_Ex7> quickCheck proptrClos
+++ OK, passed 100 tests.

--}



--Time spent: 2 hours








