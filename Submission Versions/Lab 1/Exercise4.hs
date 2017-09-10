module Lab1 where
import Data.List
import Test.QuickCheck

-- Citation 1 start
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

reversal :: Integer -> Integer
reversal = read . reverse . show
-- Citation 1 end

-- Exercise 4
exerciseFourPrimes = filter reversalIsPrime primesUnder10000
  where reversalIsPrime x = prime (reversal x)
        primesUnder10000 = takeWhile (< 10000) primes

-- I would test this function by comparing the result of the function with a list
-- that has already been verified because as the primes under 10000 are all known
-- the result is essentially a static list and the result should never change.
-- When comparing it should be noted that the order of the list is unspecified.

-- Time: 45 minutes

-- (1) Software Testing lecture slides

-- Erik van Scharrenburg
-- We chose this implementation because it requires a small amount of code and
-- the goal of the code is clear for the reader.
