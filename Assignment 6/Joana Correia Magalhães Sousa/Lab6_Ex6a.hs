module Lab6_Ex6a where
 
import Data.List
import System.Random
import Lecture6
import Lab6_Ex5

foolTestMR:: Int -> [Integer] -> IO Integer
foolTestMR k (x:xs) = do 
                      n <- primeMR k x
                      if n then return x
                      else foolTestMR k xs

{--
*Lab6_Ex5> take 20 carmichael
[294409,56052361,118901521,172947529,216821881,228842209,1299963601,
2301745249,9624742921,11346205609,13079177569,21515221081,27278026129,
65700513721,71171308081,100264053529,168003672409,172018713961,
173032371289,464052305161]
*Lab6_Ex6a> foolTestMR 1 carmichael
118901521

Carmichael number can also fool the primeMR function, but is more 
difficult to fool this function than primeTestsF.

--}
