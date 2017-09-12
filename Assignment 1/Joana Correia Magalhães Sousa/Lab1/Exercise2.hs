import Test.QuickCheck
import Data.List

--Exercise 2

calcSize:: [a] -> Int
calcSize xs = 2^(length xs)

calcSubsSize:: [a] -> Int
calcSubsSize xs = length (subsequences xs)

myTestSize:: [Int] -> Bool
myTestSize [] = True
myTestSize xs = (calcSize xs) == (calcSubsSize xs)

--For this exercise, the property is hard to test because the function
--calcSubsSize makes it difficult to calculate the number of 
--possible subsequences from a big finite list, since it can overload
--the computers memory 
--When performing the test, we're checking a mathematical fact, in 
--other words, we're checking if this property applies to every 
--possible finite lists, which one of those may have a big length
--making difficult to test such property 

--Throughout the exercises 2 to 6, they were not timed but in total I spent 7 hours
--doing all of them