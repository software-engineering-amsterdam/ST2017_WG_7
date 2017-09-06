import Test.QuickCheck
import Data.List

--Exercise 6

--I'll use the functions isPrime and getPrimes from the previous 
--exercise

findSmallestExample:: [Integer] -> Int -> Integer
findSmallestExample ps x =
        let result = foldr (*) 1 (take x ps)
        in if isPrime (result + 1)
        then findSmallestExample ps (x+1)
        else (result + 1)

getSmallestExample =
        let primes = getPrimes
        in findSmallestExample primes 2

--The smallest counterexample is 30031

--Throughout the exercises 2 to 6, they were not timed but in total I spent 7 hours
--doing all of them