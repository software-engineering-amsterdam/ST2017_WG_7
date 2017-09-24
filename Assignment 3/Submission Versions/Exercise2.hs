module Exercise2 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Exercise4

-- Exercise 2 - Joana Correia Magalhães Sousa
testParse:: Form -> Bool
testParse xs = parse (show xs) == [xs]

{--
This function gets as an input Form, converts it to string and then
apply the parse, after that, the function testParse compares the
resulted output from parse with input as a list
return true if they're equal, false otherwise
The test can be executed with 'quickCheck testParse'
--}

--Time spent: 45 mins
