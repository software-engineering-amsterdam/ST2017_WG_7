module Exercise3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Exercise 3 - Erik van Scharrenburg
cnf :: Form -> Form
cnf (Prop p) = Prop p
cnf (Neg f) = Neg (cnf f)
cnf (Cnj fs) = cnf' (sortBy cnjFirst (map cnf fs))
  where cnf' (Cnj fsc : fs) = cnf (Cnj (fs ++ fsc))
        cnf' fs = Cnj fs
cnf (Dsj fs) = cnf' (sortBy dsjFirst (map cnf fs))
  where cnf' (Cnj fsc : fs) = cnf (Cnj (map
                                        (\f -> cnf (Dsj (f : fs)))
                                        fsc))
        cnf' (Dsj fsd : fs) = cnf (Dsj (fs ++ fsd))
        cnf' fs = Dsj fs

cnjFirst (Cnj _) _ = LT
cnjFirst _ (Cnj _) = GT
cnjFirst (Dsj _) _ = LT
cnjFirst _ (Dsj _) = GT
cnjFirst _ _ = EQ

dsjFirst (Dsj _) _ = LT
dsjFirst _ (Dsj _) = GT
dsjFirst (Cnj _) _ = LT
dsjFirst _ (Cnj _) = GT
dsjFirst _ _ = EQ

-- The function converts a formula that is in negation normal form to
-- conjunctive normal form by replacing the following:
-- (P ∧ Q) ∨ R with (P ∨ R) ∧ (Q ∨ R)
-- (P ∧ Q) ∧ R with P ∧ Q ∧ R
-- (P ∨ Q) ∨ R with P ∨ Q ∨ R
-- The elements of con- and disjunctions are sorted so that the con- and
-- disjunctions are at the front.

-- Time: 6 hours
