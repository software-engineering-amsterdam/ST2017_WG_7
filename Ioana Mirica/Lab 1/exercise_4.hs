module Lab1 where
import Data.List
import Test.QuickCheck

-- Observations
--  

-- 1
prime n = 
    n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

-- 2
primes = 
    2 : filter prime [3..]

-- 3
reversal = read . reverse . show

primes_and_reversals = 
    takeWhile (< 10000) (filter (prime.reversal) primes)

-- 1 && 2 && 3 -> Lab 1

-- For exercise 1 - 5 it took me approximately 8 hours