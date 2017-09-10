module Lab1 where
import Data.List
import Data.Numbers.Primes

-- Observations
-- Smallest counterexample: [2,3,5,7,11,13]

-- 1
prime n =
    n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) my_primes

-- 2
my_primes =
    2 : filter prime [3..]

primes_list n =
    take n my_primes

conjecture_contraexample n =
    if isPrime (product(primes_list n) + 1) then conjecture_contraexample (n + 1) else primes_list n

-- 1 && 2 -> Lab 1

-- For exercise 6 - 7 it took me approximately 8 hours

-- Ioana Mirica
-- We chose this implementation because the separated functions increase the
-- code clarity.
