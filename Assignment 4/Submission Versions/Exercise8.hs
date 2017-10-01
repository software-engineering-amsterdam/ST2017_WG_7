module Exercise8 where

import Data.List
import System.Random
import Test.QuickCheck
import Exercise5
import Exercise6

-- Exercise 8 - Erik van Scharrenburg
-- There is a difference between the symmetric closure of the transitive closure
-- of a relation R and the transitive closure of the symmetric closure of R.
-- symClos (trClos r) = [(1,2),(2,1)]
-- trClos (symClos r) = [(1,1),(1,2),(2,1),(2,2)]
-- (1,1) and (2,2) are not in the symmetric closure of the transitive closure of
-- R while they are in the transitive closure of the symmetric closure of R.
exercise8 = symClos (trClos r)
         == trClos (symClos r)
  where r = [(1,2)]

-- Time: 30 minutes
