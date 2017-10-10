module Lab6_Ex3 where
 
import Data.List
import System.Random
import Lecture6
import Lab6_Ex1

divisors:: Integer -> [Integer]
divisors n = [x | x <- [1..n], mod n x == 0]

compositesJ::[Integer]
compositesJ = [x | x <- [4..], length (divisors x) >= 3]

-- Time spent: 30 mins
