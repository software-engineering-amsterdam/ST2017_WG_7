module Lab2 where
 
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

--Red Curry exercise (time start: 13:30 - 17:00, 18:40 - 19:35)

--source of information for this exercise: Lecture2 Code

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps) 


numElemsPerQuart:: Float -> Float -> [Float] -> Int
numElemsPerQuart min max xs = length (filter (\x -> ((x >= min) && (x < max))) xs)


runProbs:: Int -> IO [Int]
runProbs l = do
        val <- probs l
        let val2=numElemsPerQuart 0 0.25 val
            val3=numElemsPerQuart 0.25 0.5 val
            val4=numElemsPerQuart 0.5 0.75 val
            val5=numElemsPerQuart 0.75 1 val
        return [val2, val3, val4, val5]



check:: Integral a => [a] -> a -> a -> Bool
check [] _ _ = True
check (x:xs) min max  
        |(x >= min) && (x <= max) = check xs min max
        |otherwise = False

counting:: Float -> Float
counting l = l * 0.05

testProbs:: Int -> IO Bool
testProbs l = do
        let num = (fromIntegral l) / 4
            dif = counting (fromIntegral l)
        xs <- runProbs l
        return (check xs (floor(num - dif)) (floor(num + dif)))


--Time spent: 5 hours
{--

We do the test with 95% certainty, that is, we verify each quartile
with 5% error. With a length like 10000, we can see that the 
proportions between each quartile are as expected

--}







