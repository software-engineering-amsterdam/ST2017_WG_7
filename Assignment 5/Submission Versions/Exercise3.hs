module Exercise3 where

import Data.List
import System.Random
import Lecture5

-- Exercise3 - Joana Correia Magalhães Sousa

-- P admits unique solution
-- every P' we can get from P by erasing one of the hints admits more than one solution


isMinimal:: Node -> Bool
isMinimal (s,cs) = (uniqueSol (s,cs)) && (hasCondition (s,cs))

hasCondition:: Node -> Bool
hasCondition (s,cs) = all (\n -> (length (search succNode solved [n])) > 1) $ succNode (sud,(constraints sud))
                      where sud = eraseS s (head (filledPositions s))

generateProblem:: IO Node
generateProblem = do
                    x <- genRandomSudoku
                    y <- genProblem x
                    return y

generateListProb::Int -> IO [Node]
generateListProb 0 = return []
generateListProb x = do
        y <- getRandomInt 100
        f <- generateProblem
        fs <- generateListProb (x-1)
        return (f:fs)

tester::[Node] -> (Node -> Bool) -> IO ()
tester [] _ = putStrLn "All tested"
tester (f:fs) p = do
        tester fs p
        if p f then putStrLn "Test passed"
        else putStrLn ("Test failed ")

testProbs:: (Node -> Bool) -> IO ()
testProbs f = do
        ys <- generateListProb 100
        (tester ys f)

{--
*Lab5_Ex3> testProbs isMinimal
All tested
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test failed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test failed
Test passed
Test passed
Test passed
Test failed
Test passed
Test passed
Test passed
Test passed
Test failed
Test failed
Test passed
Test passed
Test failed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test failed
Test passed
Test passed
Test passed
Test passed
Test passed
Test failed
Test passed
Test passed
Test passed
Test passed
Test failed
Test failed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test passed
Test failed
Test passed
Test passed
Test passed
Test failed
Test passed
Test passed
Test passed
Test passed
Test passed
Test failed
Test passed
Test passed
Test passed
*Lab5_Ex3>

(This may take a while, but it works)

--}

--Time spent: 4 hours
