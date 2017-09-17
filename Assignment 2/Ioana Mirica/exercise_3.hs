module Lab2 where
import Data.List

--1
infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
forall = flip all
--1

--2
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p
--2

testDomain = [-10..10]

onAll = even
ex3one x = even x && x > 3
ex3two x = even x || x > 3
ex3three x = (even x && x > 3) || even x
ex3four x = (even x && x > 3) || even x

--f = filter onAll testDomain
--f = weaker testDomain ex3one onAll

--Time: 3h
--1&2 from lab2 and workshop2