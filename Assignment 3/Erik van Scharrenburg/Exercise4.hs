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
prop_cnf (Cnj fs) = all clause fs
prop_cnf (Dsj fs) = all literal fs
prop_cnf f = literal f

clause :: Form -> Bool
clause (Dsj fs) = all literal fs
clause f = literal f

literal :: Form -> Bool
literal (Prop _) = True
literal (Neg (Prop _)) = True
literal _ = False

-- 'quickCheck cnfTest' will check that a random formula f is equivalent to
-- cnf f and that cnf f is in conjunctive normal form.

-- Time: 1 hour and 30 minutes
