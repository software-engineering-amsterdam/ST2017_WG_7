import Test.QuickCheck
import Data.List

--Exercise 5

isPrime:: Integer -> Bool
isPrime k = null [ x | x <- [2..k - 1], mod k x  == 0]

getPrimes = [x | x <- [2..], isPrime x]

findSmallestPrime:: [Integer] -> Integer
findSmallestPrime ps =
        let result = sum (take 101 ps)
        in if isPrime result 
        then result
        else findSmallestPrime (tail ps) 

getSmallestPrime =
        let primes = getPrimes
        in findSmallestPrime primes

--To check if the answer is correct I would generate a list of prime 
--numbers, sum the first 101 elements from that list, check if the
--result is prime and less than the original answer, if true then
--the answer is wrong, otherwise I sum the next 101 consecutive
--elements from that list, sum again and compare again.
--Meanwhile, if I achieve the original answer, then it is correct

--Throughout the exercises 2 to 6, they were not timed but in total I spent 7 hours
--doing all of them