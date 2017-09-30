

module Lab4

where 

import Data.List
import Data.Char
import Test.QuickCheck
import System.Random
import Lecture2
import SetOrd
import Data.Tuple
import Test.QuickCheck.Arbitrary



-- Exercise 1
-- What would be an example of extraordinary set
-- Time spent: 50 minutes

------------- Exercise 2 ---------------------

genSetInt = do
			a <- genIntList
			return (Set a)
            

  
aa = generate (arbitrary :: Gen [Int])
ct = do ac <- generate (arbitrary :: Gen [Int]) 
        return (Set ac)

        
------------------ Exercise 3----------------------
-- Time spent: 2 hours

isintersection (Set xs) (Set ys) = Set ([x | x <- xs, elem x ys])

isunion (Set xs) (Set ys) = Set ([y | y <- ys, not (elem y xs)] ++ xs)

isdifference (Set xs) (Set ys) = Set[x | x <- xs, not (elem x ys)]

ex3 = do 
        ac1 <- generate (arbitrary :: Gen [Int])
        ac2 <- generate (arbitrary :: Gen [Int])
        print $ show $ nub ac1
        print $ show $ nub  ac2
        return $ isintersection(Set ac1) (Set ac2)
        -- "[-11,16,7,-16,12,18,-6,-28,-25,8,11,-27,1,10,-5,-21,-12]"
        -- "[29,2,-28]"
        -- {-28}


----------------------
ex5test' = do
       ac1 <- generate (arbitrary :: Gen Int)
       ac2 <- generate (arbitrary :: Gen Int)
       return [(ac1,ac2)]

ex5test = do
       ac1 <- generate (arbitrary :: Gen [Int])
       if mod (length ac1) 2 == 0 then
            return $ cnv ac1
        else do
             a <- generate (arbitrary :: Gen Int)  
             return $ nub $ cnv $ a : ac1
        

cnv [] = []        
cnv (k:v:t) = (k,v) : cnv t
      


--------------------- Exercise 5 ----------------------------
-- Time spent: 1 hour
type Rel a = [(a,a)]

symClos [] = []
symClos ((x,y):xs) =  [(x,y),(y,x)] ++ symClos xs  

testsymClos = do
              a <- ex5test
              print $ show a
              return $ symClos a

				
----------------------- Exercise 6---------------------------
-- Time spent: 2 hours

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]
-- [(1,2)] @@ [(2,5)] > [(1,5)]



trClos d = do
		  let r = d @@ d
		  let y = [aa | aa <- r, not (elem aa d)]
		  if y == [] then d
		  else sort (trClos (nub (d ++ r)))

trClostest= do
			a <- ex5test
            --print $ show a
			return $ trClos a
            
 
			

-- Exercise 7
-- Time spent: 45 minutes

prop_transitive :: Ord a => Rel a -> Bool
prop_transitive xs = length xs == length (nub (xs ++ (xs @@ xs)))

prop_symmetric :: Ord a => Rel a -> Bool
prop_symmetric xs = all (\ (x,y) -> (elem (y,x) xs)) xs



prop_1 :: Ord a => (Set a,Set a) -> Bool
prop_1 (s,t) = isdifference (isunion s t) (isdifference s t) == t 

prop_2 :: Ord a => (Set a,Set a) -> Bool
prop_2 (s,t) = isdifference (isunion s t) (isdifference t s) == s





