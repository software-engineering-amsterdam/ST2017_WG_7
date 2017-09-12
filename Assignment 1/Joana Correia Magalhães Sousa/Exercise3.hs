import Test.QuickCheck
import Data.List

--Exercise 3

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

calcPerms::Int -> Int -> Int
calcPerms x 0 = calcPerms (x - 1) x
calcPerms 0 y = y
calcPerms x y = calcPerms (x - 1) (y * x)

myTestPerms:: Int -> Bool
myTestPerms 0 = True
myTestPerms x = (length (perms (enumFromTo 1 x))) == (calcPerms x 0)

--This property is also hard to test because both functions may 
--overload the computers memory when testing with big numbers.
--In this situation we're testing whether perms satisfies a part of it
--its specification, for that we need to test various scenarios, that's
--why it get dificult to test scenarios where x is a big number

--Throughout the exercises 2 to 6, they were not timed but in total I spent 7 hours
--doing all of them