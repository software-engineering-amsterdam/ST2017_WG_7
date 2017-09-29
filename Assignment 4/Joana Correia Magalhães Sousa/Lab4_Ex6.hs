module Lab4_Ex6 where
 
import Data.List
import System.Random
import Test.QuickCheck
import Lab4_Ex5

infixr 5 @@
 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a 
trClos rel = trClosAux rel rel

trClosAux:: Ord a => Rel a -> Rel a -> Rel a
trClosAux rel1 rel2 = if (rel2 == rel3) then rel3 
                    else (trClosAux rel1 rel3)
                    where rel3 = sort(nub((rel2)++(rel1 @@ rel2)))


--Time spent: 1 hour
