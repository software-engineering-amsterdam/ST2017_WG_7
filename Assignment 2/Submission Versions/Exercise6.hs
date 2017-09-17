module Lab2 where

import Data.List
import Data.Char
import System.Random
import Data.Tuple
import Test.QuickCheck

-- Exercise 6 - Ioannis Merianos
-- Total time: 1 h
-- Specification:
-- Every letter is converted to upper case to avoid double check
-- A to M are replaced with the letter 13 places further along the alphabet
-- N to Z are replaced with the letter 13 places previous along the alphabet
-- If input is not a letter then remains the same


rot13 :: Char -> Char
rot13 c = if elem (toUpper c) "ABCDEFGHIJKLM" then chr (ord c+13)
          else if elem (toUpper c) "NOPQRSTUVWXYZ" then chr (ord c-13)
          else c

rot1 :: String -> String
rot1 []  = []
rot1 (f:r) = rot13 f : rot1 r

-- properties
property_reverse :: [Char] -> Bool
property_reverse a = a == rot1 (rot1 a)
-- property_reverse "aha" => "aha"

exercise6 = do
            quickCheck property_reverse
-- passed 100 tests
