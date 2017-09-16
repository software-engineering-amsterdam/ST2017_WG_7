module Lab2 where
 
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

--Testing properties strength

compar :: [Int] -> (Int -> Bool) -> (Int -> Bool) -> String
compar xs p q = let pq = stronger xs p q 
                    qp = stronger xs q p 
                in 
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"

forall :: [Int] -> (Int -> Bool) -> Bool
forall = flip all

stronger, weaker :: [Int] -> (Int -> Bool) -> (Int -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

prop1:: Int -> Bool
prop1 = (\ x -> even x && x > 3) 

prop2:: Int -> Bool
prop2 = (\ x -> even x || x > 3)

prop3:: Int -> Bool
prop3 = (\ x -> (even x && x > 3) || even x)

prop4:: Int -> Bool
prop4 = even 

strengthList = [(compar [(-10)..10] prop1 prop4), (compar [(-10)..10] prop2 prop4), (compar [(-10)..10] prop3 prop4), (compar [(-10)..10] prop4 prop3)]

sorting :: [String] -> [String] 
sorting [] = []  
sorting (x:xs)  
   |x == "stronger" = x:(sorting xs)
   |x == "equivalent" = x:(sorting xs)
   |x == "weaker" = (sorting xs) ++ [x]


result:: [String]
result = sorting (filter (\x -> not(x == "incomparable")) strengthList) ++ (filter (\x -> x == "incomparable") strengthList)

--Time spent: 3 hours

