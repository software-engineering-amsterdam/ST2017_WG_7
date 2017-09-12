module Lab1 where
import Data.List
import Data.Numbers.Primes

-- Observations
--  

-- 1
prime n =
    n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) my_primes

-- 2
my_primes = 
    2 : filter prime [3..]

slice start = 
   take 101 (drop start my_primes)

smallest_prime_sum start = 
    if isPrime (sum(onehundredone_primes)) then sum(onehundredone_primes) else smallest_prime_sum (start + 1)
    where onehundredone_primes = slice start

-- 1 && 2 -> Lab 1

-- For exercise 1 - 5 it took me approximately 8 hours