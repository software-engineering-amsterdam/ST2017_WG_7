module Lab6_Ex4 where
 
import Data.List
import System.Random
import Lecture6

foolTest:: Int -> [Integer] -> IO Integer
foolTest k (x:xs) = do 
                      n <- primeTestsF k x
                      if n then return x
                      else foolTest k xs

findMin:: IO Integer -> Integer -> IO Integer
findMin t 1 = t
findMin t n = do 
                x <- t
                y <- findMin t (n-1)
                return (min x y)


{--

*Lab6_Ex4> findMin (foolTest 1 composites) 100
9
*Lab6_Ex4> findMin (foolTest 2 composites) 100
9
*Lab6_Ex4> findMin (foolTest 3 composites) 100
15

The value k it's the number of cases to compare. Since the cases to 
test are chosen randomly between 2 and n, the fewer number of cases to 
compare, the higher the probability to "fool" primeTestsF with a 
coprime. The higher number of cases to number, the lower is the 
probability to "fool" primeTestsF with a coprime.
--}
