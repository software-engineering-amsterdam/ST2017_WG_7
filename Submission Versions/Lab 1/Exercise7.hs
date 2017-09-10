module Lab1 where
import Data.List
import Test.QuickCheck

-- Exercise 7
luhn :: Integer -> Bool
luhn n = mod (sum (doubleEverySecond n)) 10 == 0
  where doubleEverySecond n = reverse (doubleEverySecond' (reverse (digits n)) 1)
        doubleEverySecond' [] _ = []
        doubleEverySecond' (x:xs) i
          | even i = subNine (2 * x) : doubleEverySecond' xs (i + 1)
          | odd  i =              x  : doubleEverySecond' xs (i + 1)
          where subNine n | n > 9 = n - 9
                          | otherwise = n

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = luhn n
                   && length (digits n) == 15
                   && checkIINs n [34,37]

isMaster :: Integer -> Bool
isMaster n = luhn n
          && length (digits n) == 16
          && checkIINs n [51..55]
          && checkIINs n [2221..2720]

isVisa :: Integer -> Bool
isVisa n = luhn n
        && ( length (digits n) == 13
          || length (digits n) == 16
          || length (digits n) == 19)
        && checkIIN n 4

checkIINs :: Integer -> [Integer] -> Bool
checkIINs n = any (checkIIN n)

checkIIN :: Integer -> Integer -> Bool
checkIIN n iinToCheck = iin == iinToCheck
  where iin = undigits (take (length (digits iinToCheck)) (digits n))

digits :: Integer -> [Integer]
digits n = map digitToInteger (show n)
  where digitToInteger c = read [c] :: Integer

undigits :: [Integer] -> Integer
undigits = foldl1 (\a b -> 10 * a + b)

myLuhnTest :: Integer -> Bool
myLuhnTest n = luhn n == snd (head (filter (\x -> fst x == n) luhnTestList))
  where myLuhnTest' = forAll luhnTestListGen myLuhnTest

luhnTestListGen :: Gen Integer
luhnTestListGen = elements (map fst luhnTestList)

-- Format: (number, Boolean indicating whether number is valid)
luhnTestList :: [(Integer, Bool)]
luhnTestList = [(1, False),
                (18, True),
                (1234567890, False),
                (12345678903, True),
                (94847, False),
                (948471, True),
                (8879879987, False),
                (8879879982, True),
                (936502672, False),
                (936502673, True)]

-- Time: 3 hours

-- Erik van Scharrenburg
-- We chose this implementation because the separated functions in the where
-- blocks make the code easy to read.
