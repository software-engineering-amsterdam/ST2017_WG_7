module Lab1 where
import Data.List
import Test.QuickCheck    


-- Exercise 1a
-- quickCheck exercise1a
exercise1a n | n < 0 = True 
             | otherwise = sum (map double [1..n]) == div (n*(n+1)*(2*n+1)) 6
                where double a = a*a                        

        
-- Exercise 1b, Total Time: 3 hours, No previous experience with Haskell
-- quickCheck exercise1b
exercise1b (Positive n) = sum (map (\a -> a^3) [1..n]) == (div (n*(n+1)) 2)^2
              

-- Exercise 2
-- quickCheck exercise2
exercise2 (Positive n) = length (subsequences [1..n]) == (2^n)
-- Total Time: 30 mins
--Answer: The property is harder to test as the number of the list's elements increases.
-- This is because the computational complexity (for computing the subsets)
-- increases exponentially (2^n factor).
-- I am checking analytically a mathimatical fact for a limited number of cases (n = 39), since
-- for larger lists it is practically impossible to test.
 
 
-- Exercise 3
-- quickCheck exercise3 
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)          
            
exercise3 (Positive n) = length (perms[1..n]) == product [1..n]
-- Total time: 30 mins            
-- Answer: Again, the performance is low for larger integer lists in terms of speed
-- due to the large number of permutations and the product function.
-- I am testing a mathematical fact for a few cases (15 tests) which are true.  


--Exercise 4
reversal = read . reverse . show
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes          
primes = 2 : filter prime [3..] 

funcResevseIsPrime x = prime (reversal x)
exercise4 = filter funcResevseIsPrime (takeWhile (<10000 )primes)
--Total time: 1 hour


--Exercise 5
consPrimes all@(x:xs) = sum (take 101 all) : consPrimes (xs)

exercise5 = head $ filter prime $ consPrimes primes
-- Total time: 45 mins
-- Answer: The way this function is constructed ensures that the answer (=37447)
-- is correct. Since consecutivePrimes creates a list of sums of consecutive 101 primes,
-- and exercise5 filters the first sum that it is a prime number.

listOfSums = filter prime (consPrimes primes)
checkExercise5 = elem exercise5 listOfSums 



-- Exercise 6
out6 (Positive n) = prime ((product(take n primes)) + 1)
exercise6 = quickCheck out6
-- With quickCheck the conjecture is refuted (Returns False) when n = 6
-- i.e. (p1xp2xp3xp4xp5xp6) + 1 is not a prime number
-- Total time: 30 mins



-- Exercise 7
digits n = map (\x -> read [x] :: Int) (show n)
sub9 n = if n > 9 then n -9 else n
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x:(sub9 (2*y)):doubleEveryOther zs


myExercise7 n = mod (sum (doubleEveryOther (digits n))) 10 == 0
-- isAmericanExpress 378282246310005
isAmericanExpress n = (myExercise7 n) && ((digits n !! 0) == 3) &&
         (((digits n!! 1) == 4) || ((digits n!! 1) ==7)) && (length (digits n) == 15)
         

-- isMaster 5555555555554444
isMaster n = (myExercise7 n) && ((digits n !! 0) ==5) &&
        (digits n!!1 >0) && (digits n!!1 <6) &&
        (length (digits n) == 16)
        

isVisa n = (myExercise7 n) && ((digits n !! 0) == 4) &&
            (length (digits n) == 13 || length (digits n) == 16 || length (digits n) ==19)
-- Total Time: 1,5 hour
         

      
         
         
-- Exercise 8

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)
           

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew x = not (x==Carl) && not (x==Matthew)
accuses Peter x = x==Matthew || x==Jack
accuses Jack x = not(not (x==Carl) && not (x==Matthew)) && not(x==Matthew || x==Jack)
accuses Arnold x = xor (accuses Matthew x) (accuses Peter x)
accuses Carl x = not(accuses Arnold x)

xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

accusers :: Boy -> [Boy]
accusers is = [ b | b <- boys, accuses b is ]

guilty, honest :: [Boy]
guilty = [b | b <- boys, length (accusers b) == 3]
honest = [b | b <-boys, accuses b (guilty!!0)]

-- Total time: 1,5 hour
        
