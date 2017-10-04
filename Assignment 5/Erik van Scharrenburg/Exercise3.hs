module Exercise3 where

import Data.List
import System.Random
import Lecture5

-- Exercise 3
minimalTest :: Int -> Int -> IO ()
minimalTest i n
  | i == n    = print "Passed all tests"
  | otherwise = do r <- minimalTest'
                   if r then
                     do print ("Passed " ++ show (i + 1) ++ " test(s)")
                        minimalTest (i + 1) n
                   else
                     error "Error"
  where minimalTest' = do [r] <- rsolveNs [emptyN]
                          s  <- genProblem r
                          return (uniqueSol s)

-- Test report:
-- minimalTest 0 100
-- "Passed 1 test(s)"
-- ..
-- "Passed 100 test(s)"
-- "Passed all tests"

-- Time: 45 minutes
