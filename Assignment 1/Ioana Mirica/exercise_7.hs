module Lab1 where
import Data.List
import Data.Digits

-- Observations
-- 

luhn n = 
    if sum(map sum final_list) `mod` 10 == 0 then True else False
    where final_list = map (digits 10) (zipWith (*) (cycle [1, 2]) (digitsRev 10 n))

-- 1
fromDigits = foldl addDigit 0
   where addDigit num d = 10 * num + d

isAmericanExpress n = 
   luhn n && length (digits 10 n) == 15  && (fromDigits (take 2 (digits 10 n)) == 34 || fromDigits (take 2 (digits 10 n)) == 37)

-- 2
isMaster n = 
    luhn n && length (digits 10 n) == 16 && (True `elem` (map (fromDigits (take 2 (digits 10 n)) ==) (enumFromTo 51 55)) || True `elem` (map (fromDigits (take 4 (digits 10 n)) ==) (enumFromTo 2221 2720)))

isVisa n = 
    luhn n && (length (digits 10 n) == 13 || length (digits 10 n) == 16 || length (digits 10 n) == 19) && take 1 (digits 10 n) == [4]

-- 1 - https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
-- 2 - https://stackoverflow.com/questions/19227191/basic-haskell-how-to-check-if-a-list-of-booleans-contains-a-value

-- For exercise 6 - 7 it took me approximately 8 hours