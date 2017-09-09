module Lab1 where
import Data.List
import Test.QuickCheck

-- Observations
-- It's a property hard to test and I interrupted the execution after about 5 minutes with no result. The bigger the number we give, the bigger the number of subsequences of the list => harder and harder to compute
-- We test a mathematical fact if the number of subsequences of the list [1..n] equals 2^n

finite_set_A (Positive n) = 
    let list = [1..n] in
    length (subsequences list) == 2^n

-- For exercise 1 - 5 it took me approximately 8 hours