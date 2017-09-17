module Lab2 where
import Data.List

--1
isPermutation :: Eq a => [a] -> [a] -> Bool
--1

isPermutation la lb = 
    if length(la) == length(lb) && sort(la) == sort(lb) then True else False

--Time: ~2h