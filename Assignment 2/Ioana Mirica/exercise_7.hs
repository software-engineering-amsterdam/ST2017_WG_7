module Lab2 where
import Data.List

--1
--iban :: String -> Bool
--1

iban no = 
    map (\c -> if isAlpha c == True then  else ) no
    concat [(drop 4 no), (take 4 no)]

--Time: 1h 30min