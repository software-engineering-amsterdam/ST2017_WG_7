module Exercise6 where

import Data.List
import System.Random
import Test.QuickCheck
import Exercise5

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Exercise 6 - Joana Correia Magalhães Sousa

trClos :: Ord a => Rel a -> Rel a
trClos rel = trClosAux rel rel

trClosAux:: Ord a => Rel a -> Rel a -> Rel a
trClosAux rel1 rel2 = if (rel2 == rel3) then rel3
                    else (trClosAux rel1 rel3)
                    where rel3 = sort(nub((rel2)++(rel1 @@ rel2)))


--Time spent: 1 hour
