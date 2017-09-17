module Lab2 where

import Data.List
import Data.Char
import System.Random
import Data.Tuple
import Test.QuickCheck


-------------- Exercise 1 --------------------
-- Time spent: 1,5 hrs
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
              p <- getStdRandom random
              ps <- probs (n-1)
              return (p:ps)
              

filt025_05 x | (x >= 0.25) && (x<0.5) = True
             | otherwise = False

filt05_075 x | (x >= 0.5) && (x<0.75) = True
             | otherwise = False

exercise1 :: IO ()
exercise1 = do 
    a <- probs 10000
    let under025 = filter (<0.25) a
    let bet025_05 = filter filt025_05 a
    let bet05_075 = filter filt05_075 a
    let over075 = filter (>0.75) a
    print (length under025)
    print (length bet025_05)
    print (length bet05_075)
    print (length over075)

-- Q1,Q2,Q3,Q4
-- 2455,2473,2551,2521
-- 2460,2507,2468,2565
-- 2493,2567,2486,2454
-- 2533,2494,2491,2482

-- Since the numbers are roughly equally distributed in the 4 quartiles
-- we can safely say that the numbers are random.

    
-------------- Exercise 2 ---------------------
-- Time spent: 40 mins
data Shape = NoTriangle | Equilateral 
            | Isosceles | Rectangular | Other deriving (Eq, Show)
   
exercise2 :: Integer -> Integer -> Integer -> Shape            
exercise2 x y z | (x + y < z) || (y + z < x) || (z + x) < y = NoTriangle
          | (x == y) && (x == z) && (y == z) =  Equilateral
          | (x == y) || (x == z) || ( y == z) = Isosceles
          | (x*x + y*y) == z*z || (y*y + z*z) == x*x || (z*z + x*x) == y*y = Rectangular
          | otherwise = Other
          
-- exercise2 8 9 10 = Other
-- exercise2 8 8 8 = Equilateral
-- exercise2 8 6 6 = Isosceles
-- exercise2 3 4 5 = Rectangular
-- exercise2 3 4 8 = NoTriangle          
          
-------------- Exercise 3 -----------------------------------------------------------------
-- Time spent: 1 h
infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p


domain = [-10..10]
ex3a = do
		let f11 x = (mod x 2 == 0) && x>3
		let f12 x = (mod x 2 == 0)     
		let ff11 = length (filter f11 domain)
		let ff12 = length (filter f12 domain)
		let p11 = stronger domain f11 f12
		print ff11
		print ff12
		print p11
-- 4
-- 11
-- True (Stronger f11 than f12)


ex3b = do
		let f21 x = (mod x 2 == 0) || x>3
		let f22 x = (mod x 2 == 0)     
		let ff21 = length (filter f21 domain)
		let ff22 = length (filter f22 domain)
		let p21 = stronger domain f21 f22
		print ff21
		print ff22
		print p21
-- 14
-- 11
-- False (Weaker f21 than f22)
      
ex3c = do
		let f31 x = ((mod x 2 == 0) && x>3) || (mod x 2 == 0)
		let f32 x = (mod x 2 == 0)     
		let ff31 = length (filter f31 domain)
		let ff32 = length (filter f32 domain)
		let p31 = stronger domain f31 f32
		let p32 = weaker domain f31 f32
		print ff31
		print ff32
		print p31
		print p32
-- 11
-- 11
-- True (Stronger f31 than f32)
-- True (Weaker f31 than f32, at the same time, since they have equal number of elements)

exercise3 = do
		ex3a
		ex3b	
		ex3c

-- Judging from the results in this domain
-- Strongest --> Weakest properties are:
-- (\ x -> even x && x > 3) [4 elements]
-- even, (\ x -> (even x && x > 3) || even x), (\ x -> (even x && x > 3) || even x) [11 elements]
-- (\ x -> even x || x > 3) [14 elements]



------------------ Exercise 4 ----------------------------
-- Time spent: 1,5 h
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] ys = False
isPermutation ys [] = False

isPermutation (x:xs) ys = 
  elem x ys && isPermutation xs (delete x ys)

-- Properties
-- If the lists do not contain duplicate the test area is smaller
-- i.e. we have a stronger pre-condition
prop1 a = isPermutation a a
-- quickCheck prop1 => passed 100 tests
prop2 a = isPermutation a (sort a)
-- quickCheck prop2 => passed 100 tests
prop3 a = isPermutation a (reverse a)
-- quickCheck prop3 => passed 100 tests
prop4 a b = isPermutation a b --> sum a == sum b
-- quickCheck prop4 => passed 100 tests
prop5 a b = isPermutation a b --> isPermutation b a
-- quickCheck prop5 => passed 100 tests
ex4 = do
       print (isPermutation [1,2,3] [3,2,1])
       print (isPermutation [1,4,6,8] [4,6,8,1])
       print (isPermutation [11,44,98,66,78] [98,66,78,11,44])
       print (isPermutation [1,2] [1,2])

domain2 = [[1,2,3,4], [2,3,423],[991122,1],[45,32,6,7,8,3]]
       
ps1 = stronger domain2 prop3 prop2
-- prop1, prop2 ,prop3 are equally strong


------------ Exercise 5 ---------------------------------
-- Time spent: 2 h
isDerangement xs ys = do
            let a = zipWith (==) xs ys
            let b = elem True a
            if (isPermutation xs ys) && (not b)
            then True
            else False


deran2 xs p | elem True (zipWith (==) xs p) = False
            | otherwise = True

            
checkDeran xs (p:ps) = do 
                let a = deran2 xs p
                if a then print p else return () 
                if (length ps) > 0 then checkDeran xs (ps)
                else return()
                
deran xs = checkDeran xs (permutations xs)  

-- Properties
property_permutation a b = elem a (permutations b)
test_permutation a b = isDerangement a b --> property_permutation a b
-- quickCheck test_permutation => passed 100 tests

property_length a b = length a == length b
test_length a b = isDerangement a b --> property_length a b
-- quickCheck test_length => passed 100 tests

property_symmetric a b = isDerangement a b --> isDerangement b a          
-- quickCheck propery_symmetric => passed 100 tests

property_sort a b = isDerangement a b --> sort a == sort b
-- quickCheck property_sorted => passed 100 tests

exercise5 = do
            print (isDerangement [3,2,1] [1,3,2])
            print (isDerangement [10,20,30,4,5] [30,20,10,5,4]) -- False 20 same position
            print (isDerangement [1] [1]) -- False
            print (isDerangement [5,6,7,999] [999,5,7,6]) -- False 7 same position)
            print (isDerangement [12,23,45,776] [23,12,776,45])


---------------- Exercise 6 -------------------------------------
-- Total time: 1 h
-- Specification:
-- Every letter is converted to upper case to avoid double check
-- A to M are replaced with the letter 13 places further along the alphabet
-- N to Z are replaced with the letter 13 places previous along the alphabet
-- If input is not a letter then remains the same
 

rot13 :: Char -> Char
rot13 c = if elem (toUpper c) "ABCDEFGHIJKLM" then chr (ord c+13)
          else if elem (toUpper c) "NOPQRSTUVWXYZ" then chr (ord c-13)
          else c

rot1 :: String -> String         
rot1 []  = []        
rot1 (f:r) = rot13 f : rot1 r

-- properties
property_reverse :: [Char] -> Bool
property_reverse a = a == rot1 (rot1 a)
-- property_reverse "aha" => "aha"

exercise6 = do
            quickCheck property_reverse
-- passed 100 tests

--------------- Exercise 7 -------------------------------------------------
-- Total time: 1,5 hrs
check lng = do
            let a = take 2 lng
            if (a == "AL") && (length lng == 28) then  True 
            else if (a == "AD") && (length lng == 24) then True
            else if (a == "AT") && (length lng == 20) then True
            else if (a == "AZ") && (length lng == 28) then True
            else if (a == "BH") && (length lng == 22) then True
            else if (a == "BE") && (length lng == 16) then True
            else if (a == "BA") && (length lng == 20) then True
            else if (a == "BG") && (length lng == 22) then True
            else if (a == "HR") && (length lng == 21) then True
            else if (a == "CY") && (length lng == 28) then True
            else if (a == "CZ") && (length lng == 24) then True
            else if (a == "DK") && (length lng == 18) then True
            else if (a == "EE") && (length lng == 20) then True
            else if (a == "FO") && (length lng == 18) then True
            else if (a == "FI") && (length lng == 18) then True
            else if (a == "FR") && (length lng == 27) then True
            else if (a == "GE") && (length lng == 22) then True
            else if (a == "DE") && (length lng == 22) then True
            else if (a == "GI") && (length lng == 23) then True
            else if (a == "GR") && (length lng == 27) then True
            else if (a == "GL") && (length lng == 18) then True
            else if (a == "GT") && (length lng == 28) then True
            else if (a == "HU") && (length lng == 28) then True
            else if (a == "IS") && (length lng == 26) then True
            else if (a == "IE") && (length lng == 22) then True
            else if (a == "IL") && (length lng == 23) then True
            else if (a == "IT") && (length lng == 27) then True
            else if (a == "JO") && (length lng == 30) then True
            else if (a == "KZ") && (length lng == 20) then True
            else if (a == "KW") && (length lng == 30) then True
            else if (a == "LV") && (length lng == 21) then True
            else if (a == "LB") && (length lng == 28) then True
            else if (a == "LI") && (length lng == 21) then True
            else if (a == "LT") && (length lng == 20) then True
            else if (a == "LU") && (length lng == 20) then True
            else if (a == "MK") && (length lng == 19) then True
            else if (a == "MT") && (length lng == 31) then True
            else if (a == "MR") && (length lng == 27) then True
            else if (a == "MU") && (length lng == 30) then True
            else if (a == "MC") && (length lng == 27) then True
            else if (a == "MD") && (length lng == 24) then True
            else if (a == "ME") && (length lng == 22) then True
            else if (a == "NL") && (length lng == 18) then True
            else if (a == "NO") && (length lng == 15) then True
            else if (a == "PK") && (length lng == 24) then True
            else if (a == "PS") && (length lng == 29) then True
            else if (a == "PL") && (length lng == 28) then True
            else if (a == "PT") && (length lng == 25) then True
            else if (a == "QA") && (length lng == 29) then True
            else if (a == "RO") && (length lng == 24) then True
            else if (a == "SM") && (length lng == 27) then True
            else if (a == "SA") && (length lng == 24) then True
            else if (a == "RS") && (length lng == 22) then True
            else if (a == "SK") && (length lng == 24) then True
            else if (a == "SI") && (length lng == 19) then True
            else if (a == "ES") && (length lng == 24) then True
            else if (a == "SE") && (length lng == 24) then True
            else if (a == "CH") && (length lng == 21) then True
            else if (a == "TN") && (length lng == 24) then True
            else if (a == "TR") && (length lng == 26) then True
            else if (a == "AE") && (length lng == 23) then True
            else if (a == "GB") && (length lng == 22) then True
            else if (a == "VG") && (length lng == 24) then True
            else False


                    
move4 :: String -> String                     
move4 xs = b++a
            where 
                a = take 4 xs
                b = drop 4 xs
           
conv :: Char -> [Char] 
conv x | elem x ['A'..'Z'] = show (ord x - 55)
       | otherwise     = [x]

      
iban s | (mod (read $ concat $ map conv $ move4 s) 97 == 1) && (check s) = True
       | otherwise = False


exercise7 = do
            print (iban "AL47212110090000000235698741")
            print (iban "AD1200012030200359100100")
            print (iban "AT611904300234573201")
            print (iban "AZ21NABZ00000000137010001944")
            print (iban "BH67BMAG00001299123456")
            print (iban "BE62510007547061")
            print (iban "BA391290079401028494")
            print (iban "BG80BNBG96611020345678")
            print (iban "HR1210010051863000160")
            print (iban "CY17002001280000001200527600")
            print (iban "CZ6508000000192000145399")
            print (iban "DK5000400440116243")
            print (iban "EE382200221020145685")
            print (iban "EE0000") -- wrong => False
            print (iban "AA029332") -- wrong => False
            print (iban "RT43034985495843059384") --wrong => False
            

          


