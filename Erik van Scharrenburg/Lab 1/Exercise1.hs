module Lab1 where
import Data.List
import Test.QuickCheck

-- Exercise 1
myExerciseTwoTest (Positive n) = left == right
  where left = sum (map (^2) [1..n])
        right = div (n * (n + 1) * (2 * n + 1)) 6

myExerciseThreeTest (Positive n) = left == right
  where left = sum (map (^3) [1..n])
        right = ((n * (n + 1)) `div` 2) ^ 2

-- Time: For exercises one to three I did not keep track of the time per exercise
-- but the total time I spent on these three exercises was about 4 hours.
