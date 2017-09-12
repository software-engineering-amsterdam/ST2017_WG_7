module Lab1 where
import Data.List
import Test.QuickCheck

-- Citation 1 start
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]
-- Citation 1 end

-- Exercise 5
exerciseFivePrime = head (filter prime (sumsOfNConsecutivePrimes 101))
  where sumsOfNConsecutivePrimes n = sumsOfNConsecutivePrimes' n 0
        sumsOfNConsecutivePrimes' n i = sum (take n (drop i primes))
                                        : sumsOfNConsecutivePrimes' n (i + 1)

-- Result: 37447
-- The answer should be tested because although the answer is expected to be
-- correct it is not guaranteed to be correct.
-- It can be checked by checking whether the result is a prime and then
-- checking if there is a smaller prime number that is a sum of 101 consecutive
-- primes. There should only be 22 other possible sums that need to be checked.

-- Time: 40 minutes

-- (1) Software Testing lecture slides
