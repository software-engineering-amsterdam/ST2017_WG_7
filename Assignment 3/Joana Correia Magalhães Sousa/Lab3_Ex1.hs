module Lab3_Ex1 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

------------------Exercise 1 -------------------------

-- A contradiction is a proposition that is always false
contradiction:: Form -> Bool
contradiction f = all (\v -> not(evl v f)) (allVals f)

-- A tautology is a proposition that is always true
tautology:: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- | logical entailment
entails:: Form -> Form -> Bool
entails f1 f2 = all (\v -> evl v f2) (filter (\x -> evl x f1) (allVals f1))

-- | logical equivalence --> A and B are equivalent 
equiv:: Form -> Form -> Bool
equiv f1 f2 = (entails f1 f2) && (entails f2 f1)


--Time spent: 3 hours

{--
test1 = Cnj[p, Neg p]
test2 = Dsj[p, Neg p]
test3 = Dsj[p,q]
test4 = Cnj[p,q]
test5 = Neg(Dsj[Neg p, Neg q])

test1 is a contradiction
*Lab3> contradiction test1
True

test2 is a tautology
*Lab3> tautology test2
True

test3 does not entails test4, but test4 entails test3
*Lab3> entails test3 test4
False
*Lab3> entails test4 test3
True

test 4 is equivalent to test5
*Lab3> equiv test4 test5
True

--}
