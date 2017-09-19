module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Exercise 1
contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails a b = all (`evl` b) allOfATrue
  where allOfATrue = trueVals a ++ (allVals b \\ falseVals a)

trueVals :: Form -> [Valuation]
trueVals = foldr (\name -> map ((name, True) :)) [[]] . propNames
falseVals :: Form -> [Valuation]
falseVals = foldr (\name -> map ((name, False) :)) [[]] . propNames

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv a b = all (\x -> evl x a == evl x b) (allVals a `union` allVals b)

-- Correctness of definitions:
-- A contradiction is a formula where no valuation makes it true, in other words
-- the formula is not satisfiable which is what the contradiction function
-- checks.
-- A tautology is a formula where all valuations make it true and the tautology
-- function checks this.
-- A logically entails B if and only if B is true for all valuations where all
-- elements in A are true. The entails function checks whether this is the case.
-- A and B are equivalent if the result of A and B is the same for all
-- valuations.

-- Time: 1 hour and 45 minutes
