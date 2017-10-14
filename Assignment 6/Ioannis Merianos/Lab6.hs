module Lab6

where 

import System.Random
import Lecture6 hiding (composites)


powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

-- until e div 2 = 0 or 1

main :: IO ()
main =
  print $
  powm
    2988348162058574136915891421498819466320163312926952423791023078876139
    2351399303373464486466122544523690094744975233415544072992656881240319
    (10 ^ 40)
    1
    
    
{-    
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
  
primes :: [Integer]
primes = 2 : filter prime [3..]
-}
composites = takeWhile (\x -> x <1000) $ filter (not.prime) [3..]

{-
primeTestsF :: Int -> Integer -> IO Bool
primeTestsF k n = do
 as <- sequence $ fmap (\_-> randomRIO (2,n-1)) [1..k]
 -- sequence $ fmap (\_ -> randomRIO (2,5)) [1..4]
 -- > [5,5,2,3]
 return (all (\ a -> exM a (n-1) n == 1) as)
 -- a^p-1 mod p = 1
 -}
 
pTestF k n = do
    as <- sequence $ fmap (\_-> randomRIO (2,n-1)) [1..k]
    return (all (\ a -> powm a (n-1) n 1 == 1) as)

 
--expM ::  Integer -> Integer -> Integer -> Integer
--expM x y = rem (x^y)

--exM :: Integer -> Integer -> Integer -> Integer
--exM = expM -- to be replaced by a fast version

--------------- Exercise 4 --------------------
--testF composites
testF [] = print ""
testF (x:xs) = 
            do 
            a <- primeTestsF 2 x
            if a then
                do
                print x
                testF xs
            else 
                testF xs

                
            
-- for k = 1, the least composite that fools ferma p test is 9
-- for k = 2, 9
-- for k = 3, 301
-- for k = 4, 65
-- if we increase k, more random integers (a, lower than n)are tested,
-- thus we increase the probability of a right guess
-- note if we test 20 integers (k = 20), ferma's primality test works 100%


-------------- Exercise 5 ----------------------------

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

testC [] = print ""
testC (x:xs) = 
            do 
            a <- pTestF 3 x
            if a then
                do
                print x
                testC xs
            else 
                do
                print "not"
                testC xs 
exercise5 = testC carmichael    
-- all the carmichael numbers fool the ferma's primality test

------------- Exercise 6 -------------------------------------
testMR [] = print ""
testMR (x:xs) = 
            do 
            a <- primeMR' 2 x
            if a then
                do
                print x
                testMR xs
            else 
                do
                print "not"
                testMR xs 
                
primeMR' :: Int -> Integer -> IO Bool
primeMR' _ 2 = return True
primeMR' 0 _ = return True
primeMR' k n = do 
    a <- randomRIO (2, n-1) :: IO Integer
    if powm a (n-1) n 1 /= 1 || mrComposite a n
    then return False else primeMR' (k-1) n  
    
-- Miller Rabit algorithms test correctly if carmichael numbers are primes
-- which are not

------------- Exercise 7 -------------------------------------
ex7 [] = print ""
ex7 (x:xs) = do
            a <- primeMR' 1 (2^x - 1)
            if a then
                do
                print $ "2^" ++ show x ++ "-1" ++ "=" ++ show (2^x - 1)
                ex7 xs
             else
                do
                print $ "Is not " ++ show x
                ex7 xs
                
-- the larger Mersenne prime we can found is 2^19 -1 = 524287
-- after that we have out of memory errors    
            
 
 