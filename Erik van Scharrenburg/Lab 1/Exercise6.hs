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

-- Exercise 6
-- Result format is (n, product of [P1..Pn])
exerciseSixCounterExamples = filter (\(_, x) -> not (prime (x + 1))) primeProducts
primeProducts = primeProducts' 2
primeProducts' i = (i, product (take i primes))
                   : primeProducts' (i + 1)

-- The smallest example is when n is 6, the product will then be 30030 and 30031
-- is not a prime number.

-- Time: 40 minutes

-- (1) Software Testing lecture slides
