module Exercise6a where
import Data.List
import System.Random
import Lecture6
import Exercise5

-- Exercise 6a
millerRabinCheck = foolCheck 3 carmichael

foolCheck :: Int -> [Integer] -> IO Integer
foolCheck k (x:xs) = do b <- primeMR k x
                        if b then return x
                        else foolCheck k xs

-- The carmichael numbers can also fool the Miller-Rabin primality check but
-- with a lower probability than the Fermat primality check.

-- Time: 15 minutes
