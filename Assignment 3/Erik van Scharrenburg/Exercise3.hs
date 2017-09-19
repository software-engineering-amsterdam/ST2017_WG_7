module Exercise3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Exercise 3
cnf :: Form -> Form
cnf (Prop p) = Prop p
cnf (Neg f) = Neg (cnf f)
cnf (Cnj fs) = cnf' (sortBy (flip compare) fs)
  where cnf' (Cnj fsc : fs) = cnf (Cnj (fsc ++ fs))
        cnf' fs = Cnj (map cnf fs)
cnf (Dsj fs) = cnf' (sortBy (flip compare) fs)
  where cnf' (Cnj fsc : fs) = Cnj (map
                                  (\f -> cnf (Dsj (f : fs)))
                                   fsc)
        cnf' (Dsj fsd : fs) = cnf (Dsj (fsd ++ fs))
        cnf' fs = Dsj (map cnf fs)

-- The function converts a formula that is in negation normal form to
-- conjunctive normal form by replacing the following:
-- (P ∧ Q) ∨ R with (P ∨ R) ∧ (Q ∨ R)
-- (P ∧ Q) ∧ R with P ∧ Q ∧ R
-- (P ∨ Q) ∨ R with P ∨ Q ∨ R
-- The elements of con- and disjunctions are sorted with descending order to
-- move the con- and disjunctions to the front.

-- Time: 5 hours
