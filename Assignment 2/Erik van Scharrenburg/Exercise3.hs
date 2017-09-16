module Exercise3 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Citation 1 start
infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p
-- Citation 1 end

-- Testing properties strength
-- Exercise 3a
domain = [-10..10]

ws2ex3a1 x = even x && x > 3
ws2ex3a2   = even
ws2ex3b1 x = even x || x > 3
ws2ex3c1 x = (even x && x > 3) || even x
ws2ex3d2 x = (even x && x > 3) || even x

-- Exercise 3b
properties = [(ws2ex3a1, "even x && x > 3"),
              (ws2ex3a2, "even"),
              (ws2ex3b1, "even x || x > 3"),
              (ws2ex3c1, "(even x && x > 3) || even x"),
              (ws2ex3d2, "(even x && x > 3) || even x")]

descStrengthList = map snd (sortBy (flip strength) properties)
  where
    strength (a, _) (b, _)
      | stronger domain a b = GT
      | otherwise = LT
-- Result: ["even x && x > 3",
--          "(even x && x > 3) || even x",
--          "(even x && x > 3) || even x",
--          "even",
--          "even x || x > 3"]

-- Time: 1 hour

-- (1) Software Testing Lab 2
