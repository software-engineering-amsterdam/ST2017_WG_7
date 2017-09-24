
module Lab3

where 

import Data.List
import Data.Char
import Test.QuickCheck
import System.Random
import Lecture3



--evl [(1,True),(2,True)] form1
-- > True
-- any: at least one element of the lists satisfy the function

------------ Exercise 1 ----------------------------
satisfiable :: Form -> Bool
satisfiable f = any (\ v -> evl v f) (allVals f)

-- allVals form1 > 
-- [[(1,True),(2,True)],
-- [(1,True), (2,False)],
-- [(1,False), (2,True)]
-- [(1,False), (2,False)]
-- ]


-- Contradiction yields false for every combination of truth values
contradiction :: Form -> Bool
contradiction f = all (\ v -> not (evl v f)) (allVals f)
form4 =  Cnj [p, Neg p]
-- contradiction form4 > True

-- Tautology is true regardless of the truth-values of the statements making them
tautology :: Form -> Bool
tautology f = all (\v -> (evl v f)) (allVals f)
-- tautology form1 > True

--  B logically entails A is true if and only if it is necessary that if all of
-- the elements of B are true, then A is true.
entails f1 f2 = tautology (Equiv f1 f2)

equiv f1 f2 = tautology (Equiv f1 f2)
-- equiv form1 form2 > false

-- Total time: 2 hrs



---------------- Exercise 3 ----------------------------
-- finding the rows where the formula is False


findfalse f = filter (\ v ->  not (evl v f)) (allVals f)

aa  = findfalse form2


func1 (a,b) = if b then Neg (Prop a) else Prop a   

applyf = map Dsj $ map (map func1) $ aa


--isDsj = map Dsj applyf

isCNF = map Cnj [applyf]

-- Total time: 2,5 hrs


--testCNF :: IO Form -> IO Form
--testCNF x = map Cnj [applyf]
--                where applyf = map Dsj $ map (map func1) $ aa
--                      aa = findfalse x



---------------- Exercise 4 -----------------------------

getRandomInt = getStdRandom (randomR (1,3))

randomForm :: Int -> IO Form

randomForm 0 = do d<- getRandomInt
                  return (Prop d)
randomForm d = do q <- getRandomInt
                  d1 <- randomForm (d-1)
                  d2 <- randomForm (d-1)
                  case q of
                    1 -> return $ Neg d1
                    2 -> return $ Dsj [d1, d2]         
                    3 -> return $ Cnj [d1,d2]
                    4 -> return $ Impl d1 d2
                    5 -> return $ Equiv d1 d2


-- Total time: 3 hrs

