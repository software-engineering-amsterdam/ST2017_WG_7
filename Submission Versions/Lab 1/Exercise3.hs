module Lab1 where
import Data.List
import Test.QuickCheck

-- Observations
-- It's a property hard to test because we have to compute all the permutations of a list. As the list grows bigger, so does the number of permutations.
-- We test if the algorithm generates lists of length n! with the given input

factorial n =
    if n == 0 then 1 else n * factorial(n - 1)

test_permutations (Positive n) =
    length(permutations[1..n]) == factorial n

-- For exercise 1 - 5 it took me approximately 8 hours

-- Ioana Mirica
-- We chose this implementation because the code is easy to read and the
-- separated factorial function increases the code clarity.
