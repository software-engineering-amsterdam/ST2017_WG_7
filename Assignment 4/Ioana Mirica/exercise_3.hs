module Lab4 where
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

setsIntersection :: Ord a => Set a -> Set a -> Set a
setsUnion :: Ord a => Set a -> Set a -> Set a
setsDifference :: Ord a => Set a -> Set a -> Set a
forIntersection:: Ord a => Set a -> Set a -> Bool
forUnion:: Ord a => Set a -> Set a -> Bool
forDifference:: Ord a => Set a -> Set a -> Bool

setsIntersection (Set []) set_b = Set []
setsIntersection (Set (x:xs)) set_b 
        | inSet x set_b = insertSet x (setsIntersection (Set xs) set_b)
        | otherwise = setsIntersection (Set xs) set_b

setsUnion (Set []) set_b = set_b
setsUnion (Set (x:xs)) set_b = insertSet x (setsUnion (Set xs) (deleteSet x set_b))

setsDifference (Set []) set_b = Set []
setsDifference set_a (Set []) = set_a
setsDifference (Set (x:xs)) set_b 
        | not (inSet x set_b) = insertSet x (setsDifference (Set xs) set_b)
        | otherwise = setsDifference (Set xs) set_b

--Properties
forIntersection set_a set_b = if (subSet intersection set_a) == True && (subSet intersection set_b) == True then True else False where intersection = setsIntersection set_a set_b
forUnion set_a set_b = if (subSet set_a union) == True && (subSet set_b union) == True then True else False where union = setsUnion set_a set_b
forDifference set_a set_b = if (subSet difference set_a) == True && (subSet difference set_b) == False then True else False where difference = setsDifference set_a set_b