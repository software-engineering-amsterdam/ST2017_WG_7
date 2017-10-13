module Lab6_Ex5 where
 
import Data.List
import System.Random
import Lecture6
import Lab6_Ex4

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

{--Carmichael number is a composite number n which satisfies the modular arithmetic congruence relation: b^(n-1) equivalent to 1 (mod n) for all integers b which are relatively prime to n.

*Lab6_Ex5> take 20 carmichael
[294409,56052361,118901521,172947529,216821881,228842209,1299963601,
2301745249,9624742921,11346205609,13079177569,21515221081,27278026129,
65700513721,71171308081,100264053529,168003672409,172018713961,
173032371289,464052305161]
*Lab6_Ex5>
*Lab6_Ex5> foolTest 50 carmichael
294409


With the carmichael number n, the chance to chose a prime number 
randomly between 2 and n is higher. Even with a high value of k, it
can still fool primeTestsF . It is easier to fool primeTestsF with a
carmicheal number

--}


