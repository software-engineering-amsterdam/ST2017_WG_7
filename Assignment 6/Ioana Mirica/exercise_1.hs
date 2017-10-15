module Lab6 where

exM :: Integer -> Integer -> Integer -> Integer
exM x y n = 
    if n == 1 then x `rem` n else untilPoint x 1 y n

untilPoint :: Integer -> Integer -> Integer -> Integer -> Integer
untilPoint x e y n = 
    if y == 1 then (x*e) `rem` n else untilPoint x ((x*e) `rem` n) (y-1) n

--Time spent: ~2h