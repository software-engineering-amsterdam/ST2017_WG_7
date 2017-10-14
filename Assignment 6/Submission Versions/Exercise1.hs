module Exercise1 where
import Data.List
import System.Random

-- Exercise 1 - Erik van Scharrenburg
exM :: Integer -> Integer -> Integer -> Integer
exM x e m
  | p == 0    = (x ^ e) `rem` m
  | otherwise = (sqM x p * exM x r m) `rem` m
  where r = e `rem` t
        t = 2 ^ p
        p = largestPow2 e
        sqM x 0 = x
        sqM x i = (sqM x (i - 1) ^ 2) `rem` m

largestPow2 x = lp2 x 1
  where lp2 x i
          | 2 ^ i > x = i - 1
          | otherwise = lp2 x (i + 1)

-- Time: 1 hour
