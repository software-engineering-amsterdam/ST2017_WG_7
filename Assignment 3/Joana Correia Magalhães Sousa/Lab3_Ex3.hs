module Lab3_Ex3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


----------------------------- Exercise 3 ------------------------------

{--
   First step is to remove the arrows
   Second step is convertion to negation normal form
   If it is in dnf, then apply De Morgan law, otherwise, just return
   the result
   !!It is not possible to turn dnf to cnf if it is a tautology!!
--}



deMorgan:: Form -> Form -> Form
deMorgan (Cnj [f1]) f2 = deMorgan f1 f2
deMorgan f1 (Cnj [f2]) = deMorgan f1 f2
deMorgan (Cnj(f:f1)) f2 = Cnj[(deMorgan f f2), (deMorgan (Cnj f1) f2)]
deMorgan f1 (Cnj(f:f2)) = Cnj[(deMorgan f1 f), (deMorgan f1 (Cnj f2))]
deMorgan f1 f2 = Dsj[f1,f2] 

convert:: Form -> Form
convert (Dsj[f1,f2]) = deMorgan f1 f2
convert f = f

cnf:: Form -> Form
cnf = toStart . convert . nnf . arrowfree

allNormal::[Form] -> [Form]
allNormal [] = []
allNormal ((Cnj f):fs) = ((allNormal f)++(allNormal fs))
allNormal ((Dsj f):fs) = ([Dsj(allNormalDsj f)]++(allNormal fs))
allNormal (f:fs) = (f:(allNormal fs))

allNormalDsj::[Form] -> [Form]
allNormalDsj [] = []
allNormalDsj ((Dsj f):fs) = ((allNormalDsj f)++(allNormalDsj fs))
allNormalDsj (f:fs) = (f:(allNormalDsj fs))

toStart::Form -> Form
toStart (Cnj f) = (Cnj(allNormal f))
toStart f =f 


--Time spent: 4 hours
