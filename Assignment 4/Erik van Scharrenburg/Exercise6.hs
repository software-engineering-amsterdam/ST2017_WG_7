module Exercise6 where

import Data.List
import System.Random
import Test.QuickCheck
import Exercise5

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Exercise 6
trClos :: Ord a => Rel a -> Rel a
trClos a
  | a == tr   = tr
  | otherwise = trClos tr
  where tr = nubSort (a ++ a @@ a)


-- Time: 15 minutes
