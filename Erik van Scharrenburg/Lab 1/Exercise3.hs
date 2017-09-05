module Lab1 where
import Data.List
import Test.QuickCheck

-- Exercise 3
myExerciseFiveTest (Positive n) = left == right
  where left = length (permutations [1..n])
        right = fac n
          where fac 0 = 1
                fac x = x * fac (x - 1)

-- The property is hard to test because it takes a lot of time to compute all
-- the permutations of a large list.
-- The test is testing whether the permutations function produces lists with a
-- length of n! with an input of [1..n].

-- Time: For exercises one to three I did not keep track of the time per exercise
-- but the total time I spent on these three exercises was about 4 hours.
