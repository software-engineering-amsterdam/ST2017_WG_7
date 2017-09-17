module Lab2 where

import Data.List
import Data.Char
import System.Random
import Data.Tuple
import Test.QuickCheck

-- Exercise 3 - Ioannis Merianos
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
