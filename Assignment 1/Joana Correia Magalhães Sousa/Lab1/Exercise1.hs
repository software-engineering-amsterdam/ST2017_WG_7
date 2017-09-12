import Test.QuickCheck
import Data.List

--Exercise 1

--For the first quickCheck
mySum:: Int-> Int
mySum x = sum (map (^2) (enumFromTo 1 x))

calc:: Int -> Int
calc x = div (x * (x+1) * (2*x + 1)) 6

myTest:: Int -> Bool
myTest 0 = True
myTest x = (mySum x) == (calc x)

--For the second quickCheck
mySum3::Int -> Int
mySum3 x = sum (map (^3) (enumFromTo 1 x))

calc3::Int -> Int
calc3 x = (div (x * (x + 1)) 2)^2

myTest3::Int -> Bool
myTest3 0 = True
myTest3 x = (mySum3 x) == (calc3 x)

--The time spent on this exercise was around 3 hours