module Exercise6a where
import Data.List
import System.Random
import Lecture6
import Exercise5

-- Exercise 6a - Ioannis Merianos
testMR [] = print ""
testMR (x:xs) =
            do
            a <- primeMR' 2 x
            if a then
                do
                print x
                testMR xs
            else
                do
                print "not"
                testMR xs

primeMR' :: Int -> Integer -> IO Bool
primeMR' _ 2 = return True
primeMR' 0 _ = return True
primeMR' k n = do
    a <- randomRIO (2, n-1) :: IO Integer
    if powm a (n-1) n 1 /= 1 || mrComposite a n
    then return False else primeMR' (k-1) n

powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

-- testMR carmichael
-- Miller Rabit algorithms test correctly if carmichael numbers are primes
-- which are not
-- Time: 45 minutes
