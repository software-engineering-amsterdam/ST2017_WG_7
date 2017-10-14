module Exercise4 where
import Data.List
import System.Random
import Lecture6

-- Exercise 4 - Erik van Scharrenburg
exercise4 :: Int -> IO Integer
exercise4 k = minNtests (foolCheck k composites) 100

foolCheck :: Int -> [Integer] -> IO Integer
foolCheck k (x:xs) = do b <- primeTestsF k x
                        if b then return x
                        else foolCheck k xs

minNtests :: (Eq t, Num t, Monad m, Ord a) => m a -> t -> m a
minNtests f 1 = f
minNtests f n = do a <- f
                   b <- minNtests f (n - 1)
                   return (min a b)

foolCheck9 :: Int -> IO ()
foolCheck9 k = fc9 k 1
  where fc9 k i = do b <- primeTestsF k 9
                     if b then print
                       ("Fooled the test after " ++ show i ++ " attempts.")
                     else
                       fc9 k (i + 1)

-- The smallest composite number I found that can fool the test is 9, this is
-- with k = 1, k = 2 and k = 3.
-- If k is increased the probability that a small composite number will fool the
-- test will decrease but since the test is random a composite number that can
-- fool one test (like 9) could fool any number of tests as demostrated by the
-- foolCheck9 function. It will keep trying to fool the test with the number 9.
-- In some quick tests the test with k = 6 can be fooled after ~60.000 attempts,
-- with k = 7 after ~350.000 attempts and with k = 8 after ~6 million attempts.

-- Time: 1 hour and 15 minutes
