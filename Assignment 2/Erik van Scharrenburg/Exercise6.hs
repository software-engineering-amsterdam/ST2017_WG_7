module Exercise6 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Data.Maybe

-- Implementing and testing ROT13 encoding
-- Exercise 6
-- Specification:
-- The alphabet is the basic Latin alphabet.
-- A letter is a letter from the alphabet.
-- The function should accept a String as input.
-- The function should output a String that is the same as the input string
-- except every letter is replaced by the letter 13 places after it in the
-- alphabet.

-- Implementation:
rot13 :: String -> String
rot13 = map rot13c

rot13c :: Char -> Char
rot13c c
  | isJust index1 = ys !! fromJust index1
  | isJust index2 = xs !! fromJust index2
  | otherwise = c
  where index1 = elemIndex c xs
        index2 = elemIndex c ys
        xs = ['a'..'m'] ++ ['A'..'M']
        ys = ['n'..'z'] ++ ['N'..'Z']

-- Time: 30 minutes.
