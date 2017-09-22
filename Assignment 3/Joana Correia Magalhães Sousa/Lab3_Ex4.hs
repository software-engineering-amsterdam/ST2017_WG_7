module Lab3_Ex4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Lab3_Ex1
import Lab3_Ex3


-------------------------------Exercise 4 -----------------------------

getRandomInt :: Int -> Int-> IO Int
getRandomInt m n = getStdRandom (randomR (m,n))


getRandomForms:: Int -> Int -> IO [Form]
getRandomForms 0 _ = return []
getRandomForms c d = do
        f <- getForm d
        fs <- getRandomForms (c-1) d
        return (f:fs)
{--
   There's a random variable that goes between 0 and 5, depending on
   the result, the function returns:
   0 -> Prop
   1 -> Neg 
   2 -> Cnj
   3 -> Dsj
   4 -> Impl
   5 -> Equiv

--}

getForm :: Int -> IO Form
getForm 0 = do
        x <- getRandomInt 0 3
        return (Prop x)
getForm d = do 
        x <- getRandomInt 0 5
        y <- getRandomInt 0 3
        z <- getRandomInt 2 5
        f1 <- getForm (d-1)
        f2 <- getForm (d-1) 
        fs <- getRandomForms z (d-1) 

        if (x == 0) then return (Prop y)
        else if (x == 1) then return (Neg f1)
        else if (x == 2) then return (Cnj fs)
        else if (x == 3) then return (Dsj fs)
        else if (x == 4) then return (Impl f1 f2)
        else return (Equiv f1 f2)



------------------------------------------------------------------------

--After applying the cnf function, the result and its original form
--must be equivalent
property1:: Form -> Bool
property1 f = equiv (cnf f) f

--After applying the cnf function, the result must be a cnj of dsj
--property2

{--tester::[Form] -> (Form -> Bool) -> IO ()
tester [] _ = putStrLn "All passed"
tester (f:fs) p = do
        tester fs p
        if p f then putStrLn "Test passed"
        else putStrLn ("Test failed: " ++ show f)

generateListForms::Int -> IO [Form]
generateListForms 0 = return []
generateListForms x = do
        y <- getRandomInt 0 10
        f <- getForm y
        fs <- generateListForms (x-1)
        return (f:fs)

testForms:: (Form -> Bool) -> IO ()
testForms f = do
        ys <- generateListForms 10
        (tester ys f)--}

--Time spent: 5 hours


