module Lab1 where
import Data.List
import Test.QuickCheck

-- Exercise 2
-- quickCheck exercise2
exercise2 (Positive n) = length (subsequences [1..n]) == (2^n)
-- Total Time: 30 mins
-- Answer: The property is harder to test as the number of the list's elements increases.
-- This is because the computational complexity (for computing the subsets)
-- increases exponentially (2^n factor).
-- I am checking analytically a mathimatical fact for a limited number of cases (n = 39), since
-- for larger lists it is practically impossible to test.

-- Ioannis Merianos
-- We chose this implementation because it requires a small amount of code and
-- the goal of the code is clear for the reader.
