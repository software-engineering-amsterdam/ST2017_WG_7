module Exercise2 where
import Data.List
import System.Random
import System.CPUTime
import Lecture6 -- modified to include Exercise1

-- Exercise 2
testList :: Integer -> Integer -> Integer -> IO [(Integer, Integer, Integer)]
testList 0 _   _   = return []
testList n min max = do x  <- randomRIO (min, max)
                        e  <- randomRIO (min, max)
                        m  <- randomRIO (min, max)
                        xs <- testList (n - 1) min max
                        return ((x,e,m):xs)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

getTime :: Num t => (a -> b -> c -> t, [(a, b, c)]) -> IO ()
getTime (f, ts) = do seq ts (return ())
                     t1 <- getCPUTime
                     seq (foldl seq 0 (map (uncurry3 f) ts)) (return ())
                     t2 <- getCPUTime
                     print (show ((t2 - t1) `div` 10^6) ++ "ms")

exercise2 :: IO ()
exercise2 = do ts <- testList 100 1 (2^20)
               print "exM:"
               getTime (exM, ts)
               print "expM:"
               getTime (expM, ts)

-- With a test list of 100 integers and a max bound of 2^20 the exM function is
-- around 100 times faster than the expM function.
-- With a max bound of 2^64 even one test of the expM function will often take
-- too long to be feasable.

-- Time: 1 hour and 30 minutes
