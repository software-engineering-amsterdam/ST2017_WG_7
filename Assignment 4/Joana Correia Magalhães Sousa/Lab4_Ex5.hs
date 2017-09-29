module Lab4_Ex5 where
 
import Data.List
import System.Random
import Test.QuickCheck


type Rel a = [(a,a)] 


symClos:: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):xs) = [(x,y),(y,x)] ++ (symClos xs)


--Time spent: 45 mins
