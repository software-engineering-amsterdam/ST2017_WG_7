import Test.QuickCheck
import Data.List

--Exercise 7

double:: Integer -> [Integer] -> [Integer] -> [Integer]
double 0 [] (y:ys) = double 1 [y] ys
double _ xs [] = xs
double i xs (y:ys)
        |(odd i) && ((2*y) >= 10) = double (i+1) (xs ++ [(y*2)-9]) ys
        |(odd i) && ((2*y) < 10) = double (i+1) (xs ++ [(y*2)]) ys
        |otherwise = double (i+1) (xs ++ [y]) ys

invertAndCalc:: [Integer] -> [Integer]
invertAndCalc xs = reverse (double 0 [] (reverse xs))

numToList:: Integer -> [Integer]
numToList 0 = []
numToList x = numToList (div x 10) ++ [mod x 10]

listToNum :: [Integer] -> Integer
listToNum xs = foldl (\num x -> 10*num + x) 0 xs

luhn::Integer -> Bool
luhn x  
        |(mod result 10) == 0 = True
        |otherwise = False
        where result = sum (invertAndCalc (numToList x))

isAmericanExpress::Integer -> Bool
isAmericanExpress x = 
        let number = numToList x
        in ((length number) == 15) && (checkAE (take 2 number)) && (luhn x)


isMaster::Integer -> Bool
isMaster x = 
        let number = numToList x
        in (length number) == 16 && ((checkM (take 2 number)) || (checkMs (take 4 number))) && (luhn x)


isVisa::Integer -> Bool
isVisa x = 
        let number = numToList x
        in (((length number) == 13) || ((length number) == 16) || (length number) == 19) && ((take 1 number) == [4]) && (luhn x)


checkM:: [Integer] -> Bool
checkM xs
        |elem num [51..55] = True
        |otherwise = False
        where num = listToNum xs

checkMs:: [Integer] -> Bool
checkMs xs
        |elem num [2221..2720] = True
        |otherwise = False
        where num = listToNum xs

checkAE:: [Integer] -> Bool
checkAE xs 
        |((num == 34) || (num == 37)) = True
        |otherwise = False
        where num = listToNum xs             

--The time spent on this exercise was around 5 hours