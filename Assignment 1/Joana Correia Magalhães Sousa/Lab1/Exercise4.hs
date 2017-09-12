import Test.QuickCheck
import Data.List

--Exercise 4

reversal :: Integer -> Integer
reversal = read . reverse . show

removeMultiples:: Integer -> [Integer] -> [Integer] -> [Integer]
removeMultiples x ys [] = ys
removeMultiples x [] (z:zs)
        |mod z x == 0 = removeMultiples x [] zs
        |otherwise = removeMultiples x [z] zs
removeMultiples x ys (z:zs)
        |mod z x == 0 = removeMultiples x ys zs
        |otherwise = removeMultiples x (ys ++ [z]) zs

getPrimesFinite::[Integer] -> [Integer] -> [Integer]
getPrimesFinite [] (y:ys) = getPrimesFinite [y] (removeMultiples y [] ys)
getPrimesFinite xs (y:ys) = getPrimesFinite (xs ++ [y]) (removeMultiples y [] ys)
getPrimesFinite xs [] = xs

getReversedPrimes = 
        let primes = getPrimesFinite [] (enumFromTo 2 10000)
        in map reversal (filter (\x -> elem x primes) (map reversal primes))

--To test this function, I would pick positive integer number x,
--generate a list from 2 to x, apply the result on a similar function
--to getReversedPrimes, and verify if each element and it's reverse
--are both primes

--Throughout the exercises 2 to 6, they were not timed but in total I spent 7 hours
--doing all of them