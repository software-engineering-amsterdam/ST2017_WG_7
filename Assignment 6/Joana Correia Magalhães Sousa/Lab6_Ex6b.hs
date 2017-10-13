module Lab6_Ex6b where
 
import Data.List
import System.Random
import Lecture6

mersennePrime:: [Integer] -> IO ()
mersennePrime (p:ps) = do
                         let test = (2 ^ p) - 1
                         x <- primeMR 10 test
                         if x then (print (p, test))
                         else return ()
                         mersennePrime ps

{--

*Lab6_Ex6b> mersennePrime primes
(2,3)
(3,7)
(5,31)
(7,127)
(13,8191)
(17,131071)
(19,524287)
GNU MP: Cannot allocate memory (size=1006764048)
Aborted (core dumped)

I wasn not able to check the rest of the primes, but with the few 
results that I got, we can check they are genuine thanks to the 
website https://www.mersenne.org/primes/ 

Time spent: 45 mins

--}
