module Lab2 where
 
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

--Implementing and testing ROT13 encoding

{-- From a given string x, the function will transform every letter in 
x by replacing it with a letter that is 13 positions ahead in the 
alphabet, wrapping back to the beginning if necessary.
--}

changeChar x 
        | (x >= 'a') && (x <= 'z') && (ltr > 'z') = rslt
        | (x >= 'a') && (x <= 'z') && (ltr <= 'z') = ltr
        | (x >= 'A') && (x <= 'Z') && (ltr > 'Z') = rslt
        | (x >= 'A') && (x <= 'Z') && (ltr <= 'Z') = ltr
        | otherwise = x
        where ltr = chr((ord x)+13)
              rslt = chr((ord ltr)-26)


rot13::[Char] -> [Char]
rot13 [] = []
rot13 (x:xs) = (changeChar x):(rot13 xs)

--The encoded string must have the same size as the original
property1:: String -> Bool
property1 xs = (length xs) == (length (rot13 xs))


--The encoded string from the original encoded string must be the original
property2:: String -> Bool
property2 xs = xs == (rot13 (rot13 xs))

--If we reverse the original string and apply rot13, it will be the same as apllying first the rot13 on the original string and then reverse
property3 :: [Char] -> Bool
property3 xs = rot13 (reverse xs) == reverse (rot13 xs)

--Time spent: 3 hours





