module Exercise3 where

import Data.List
import System.Random

-- Exercise 3 - Joana Correia Magalhães Sousa
divisors:: Integer -> [Integer]
divisors n = [x | x <- [1..n], mod n x == 0]

composites::[Integer]
composites = [x | x <- [4..], length (divisors x) >= 3]

-- Time spent: 30 mins
