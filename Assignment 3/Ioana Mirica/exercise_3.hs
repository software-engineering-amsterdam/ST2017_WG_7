module Lab3 where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import exercise_1

--1
-- Eliminate <->, replacing p <-> q with (p -> q) ∧ (q -> p)
-- Eliminate ->, replacing p -> q with ¬p ∨ q
-- Move negation (¬) inward, replace ¬(p ∨ q) with ¬p ∧ ¬q
-- Distribute ∨, replace p ∨ (q ∧ r) with (p ∨ q) ∧ (p ∨ r)
--1

convertToCNF :: Form -> Form
cnf :: Form -> Form

cnf (Dsj [l, r]) = deMorgan l r
cnf (Cnj [l, r]) = deMorgan l r
    

convertToCNF f = 
    cnf(nnf(arrowfree f))