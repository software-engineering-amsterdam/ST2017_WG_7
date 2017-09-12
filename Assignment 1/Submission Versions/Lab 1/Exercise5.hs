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

--Exercise 5
consPrimes all@(x:xs) = sum (take 101 all) : consPrimes (xs)

exercise5 = head $ filter prime $ consPrimes primes
-- Total time: 45 mins
-- Answer: The way this function is constructed ensures that the answer (=37447)
-- is correct. Since consecutivePrimes creates a list of sums of consecutive 101 primes,
-- and exercise5 filters the first sum that it is a prime number.

listOfSums = filter prime (consPrimes primes)
checkExercise5 = elem exercise5 listOfSums

-- (1) Software Testing lecture slides

-- Ioannis Merianos
-- We chose this implementation because it requires a small amount of code and
-- the goal of the code is clear for the reader.
