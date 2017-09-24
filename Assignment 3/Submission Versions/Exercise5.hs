module Exercise5 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Exercise1
import Exercise2
import Exercise3

type Clause  = [Int]
type Clauses = [Clause]

-- Exercise 5 - Erik van Scharrenburg
cnf2cls :: Form -> Clauses
cnf2cls (Prop p) = [[p]]
cnf2cls (Neg (Prop p)) = [[-p]]
cnf2cls (Cnj fs) = concatMap cnf2cls fs
cnf2cls (Dsj fs) = [concat (concatMap cnf2cls fs)]

cls2cnf :: Clauses -> Form
cls2cnf cs = cnj (map (dsj . map prop) cs)
  where cnj [x] = x
        cnj xs  = Cnj xs
        dsj [x] = x
        dsj xs  = Dsj xs
        prop x | x < 0 = Neg (Prop (-x))
               | otherwise = Prop x

clsTest :: Form -> Bool
clsTest f = cnff `equiv` clsf
  where cnff = (cnf . nnf . arrowfree) f
        clsf = (cls2cnf . cnf2cls) cnff

-- 'quickCheck clsTest' checks that f in conjunctive normal form is equivalent
-- to f converted to cnf, converted to clause form and converted back to cnf.
-- The test depends on cls2cnf being correct.

-- Time: 1 hour and 15 minutes
