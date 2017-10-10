module Exercise5 where
import Data.List
import System.Random
import Lecture6
import Exercise4

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]

-- Exercise 5
exercise5 k = foolCheck k carmichael

-- The numbers in the carmichael list will pass most of the tests and can thus
-- fool the test even with a very high k.

-- Time: 45 minutes
