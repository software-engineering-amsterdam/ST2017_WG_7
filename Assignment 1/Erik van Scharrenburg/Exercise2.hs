module Lab1 where
import Data.List
import Test.QuickCheck

-- Exercise 2
myExerciseFourTest (Positive n) = left == right
  where left = length (subsequences [1..n])
        right = 2^n

-- The property is hard to test because it takes a lot of time to compute all
-- the subsequences of a large list.
-- The test is testing whether the subsequences function produces lists with a
-- length of 2^n with an input of [1..n].

-- Time: For exercises one to three I did not keep track of the time per exercise
-- but the total time I spent on these three exercises was about 4 hours.
