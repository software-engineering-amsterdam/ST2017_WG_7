module Exercise1 where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Exercise 1 - Ioana Mirica
--checked manually by truth table
formContradiction = Cnj [p, Neg p]
formTautology = Dsj [p, Neg p]
--form1 & form3 are tautologyes and form2 is satisfiable

--1
--satisfiable :: Form -> Bool
--satisfiable f = any (\ v -> evl v f) (allVals f)
--1

--2
contradiction :: Form -> Bool
tautology :: Form -> Bool
entails :: Form -> Form -> Bool
equiv :: Form -> Form -> Bool
--2

--Contradiction -> all evaluation of the formula are false
contradiction f = all (\ v -> not(evl v f)) (allVals f)

--Tautology -> all evaluation of the formula are true
tautology f = all (\ v -> evl v f) (allVals f)

--Entailment -> A entails B if, whenever A is true, B must also be true
valuationList :: Form -> [Bool]
valuationList f = zipWith evl (allVals f) (replicate len f) where len = length(allVals f)
entails f1 f2 = if sum checker > 0 then False else True where checker = zipWith (\x y -> if x == True && y == False then 1 else 0) listf1 listf2
                                                              listf1 = valuationList f1
                                                              listf2 = valuationList f2

--Equivalence -> if ((f1->f2) ^ (f2->f1)) is a tautology => f1 <==> f2
equiv f1 f2 = tautology (equ) where equ = Cnj[(Impl f1 f2), (Impl f2 f1)]

--Citations: 1 -> Lecture 3 slides
--			 2 -> Lab 3
--Checked manually by truth table and then apply in practice and observe if I get the same answers
--Time: 7h30min
