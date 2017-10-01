module Exercise7 where

import Data.List
import System.Random
import Test.QuickCheck
import Exercise5
import Exercise6

-- Exercise 7 - Erik van Scharrenburg
testSC :: Int -> Int -> IO ()
testSC i n
  | i == n    = print ("Passed " ++ show n ++ " tests")
  | otherwise = do r <- genRel
                   if prop_isSymmetricClosure (symClos r) r then
                     testSC (i + 1) n
                   else
                     error ("Error on " ++ show r)
  where genRel = do n1 <- getStdRandom (randomR (0, 10))
                    n2 <- getStdRandom (randomR (0, 10))
                    xs <- randomIntL n1
                    ys <- randomIntL n2
                    return (zip xs ys)
        randomIntL :: Int -> IO [Int]
        randomIntL 0 = return []
        randomIntL n = do x  <- getStdRandom (randomR (1, 100))
                          xs <- randomIntL (n - 1)
                          return (x : xs)

-- Own test method
testSymClos :: IO ()
testSymClos = testSC 0 100

-- QuickCheck test
testQCsymClos :: IO ()
testQCsymClos = quickCheck (\r -> prop_isSymmetricClosure (symClos r) r)

-- Prop checking that:
-- - rs contains r
-- - rs is symmetric
-- - there is no subset of rs that is symmetric and contains r
prop_isSymmetricClosure :: Rel Int -> Rel Int -> Bool
prop_isSymmetricClosure rs r = subset r rs
                            && symmetric rs
                            && not (any (\xs -> subset r xs
                                             && symmetric xs) (subsets rs))
  where subset r rs = all (`elem` rs) r
        symmetric r = all (\x -> swap x `elem` r) r
        subsets xs    = [x | x <- subsequences xs, length x < length xs]

-- Time: 2 hours
