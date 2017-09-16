module Lab2 where
 
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


--Recognizing and generating derangements

isDerangement:: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement xs [] = False
isDerangement [] ys = False
isDerangement (x:xs) (y:ys)
        |x == y = False
        |otherwise = isDerangement xs ys

deran:: Eq a => [a] -> [[a]]
deran [] = []
deran ys = filter (\x -> isDerangement x ys) (permutations ys)

--A list is not a derangement of itself
property1::[Int] -> [Int] -> Bool
property1 xs ys = (isDerangement xs ys) --> not(xs == ys)


testR5 :: Int -> Int -> ([Int] -> [Int] -> Bool) -> IO ()
testR5 k n f = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList 
                  ys <- genIntList
                  if (f xs ys) then
                    do print ("pass on: " ++ show xs ++ show ys)
                       testR5 (k+1) n f
                  else error ("failed test on: " ++ show xs ++ show ys)

--Time spent: 2 and a half hours












