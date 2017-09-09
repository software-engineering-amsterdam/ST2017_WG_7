module Lab1 where
import Test.QuickCheck

-- Observations
-- Positive is a QuickTest modifier that guarantees, in our case, that n will always be selected as an integer number bigger than 0 for any given test (https://hackage.haskell.org/package/QuickCheck-2.10.0.1/docs/Test-QuickCheck-Modifiers.html)
-- use enumFromTo because we always know that our sum starts with 1 and goes until a given n
-- div - use div to perform Integer division (requires arguments whose type is in the class Integral). If we use / arguments whose type is in the class Fractional is required and performs standard division (https://stackoverflow.com/questions/7368926/division-in-haskell)

-- Part 1
sum_of_n_2 (Positive n) =
   sum (map (^2) (enumFromTo 1 n)) == div (n * (n + 1) * (2 * n + 1)) 6
   
-- Part 2
sum_of_n_3 (Positive n) = 
    sum (map (^3) (enumFromTo 1 n)) == (div (n * (n + 1)) 2) ^ 2

-- For exercise 1 - 5 it took me approximately 8 hours