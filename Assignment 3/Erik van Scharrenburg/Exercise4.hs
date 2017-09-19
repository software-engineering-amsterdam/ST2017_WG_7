module Exercise4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Exercise1
import Exercise2
import Exercise3

-- Exercise 4
-- The Form generator is defined in Exercise2
cnfTest :: Form -> Bool
cnfTest f = prop_equiv f cnff
         && prop_cnf cnff
  where cnff = (cnf . nnf . arrowfree) f

prop_equiv :: Form -> Form -> Bool
prop_equiv a b = a `equiv` b

prop_cnf :: Form -> Bool
prop_cnf (Cnj fs) = all propOrNegProp fs
prop_cnf (Dsj fs) = all propOrNegProp fs
prop_cnf f = propOrNegProp f

propOrNegProp :: Form -> Bool
propOrNegProp (Prop _) = True
propOrNegProp (Neg (Prop _)) = True
propOrNegProp _ = False

-- 'quickCheck cnfTest' will check that a random formula f is equivalent to
-- cnf f and that cnf f is in conjunctive normal form.

-- Time: 1 hour and 15 minutes
