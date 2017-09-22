module Lab3_Ex2 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

testParse:: Form -> Bool
testParse xs = parse (show xs) == [xs]

{--
This function gets as an input Form, converts it to string and then
apply the parse, after that, the function testParse compares the 
resulted output from parse with input as a list
return true if they're equal, false otherwise
--}

--Time spent: 45 mins
