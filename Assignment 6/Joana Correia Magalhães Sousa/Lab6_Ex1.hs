module Lab6_Ex1 where
 
import Data.List
import System.Random
import Lecture6

exM :: Integer -> Integer -> Integer -> Integer
exM x y n = rem (exMAux x y n) n
            where exMAux x 1 _ = x
                  exMAux x y n
                        | odd y = rem (x * (exMAux (x^2) (div (y-1) 2) n)) n
                        | otherwise = rem (exMAux (x^2) (div y 2) n) n 

-- Time spent: 2 hours and 30 mins
