module Exercise1 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Citation 1 start
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)
-- Citation 1 end

-- Red Curry
-- Exercise 1 - Erik van Scharrenburg
doChiSq :: IO Float
doChiSq = do xs <- probs 1000000
             return (chiSq xs)

chiSq :: [Float] -> Float
chiSq observations = sum (map ((/ e) . (^2) . subtract e . fromIntegral)
                            (toList (counts observations)))
  where e = fromIntegral (length observations) / 4 :: Float

toList :: (a, a, a, a) -> [a]
toList (a, b, c, d) = [a, b, c, d]

counts :: [Float] -> (Int, Int, Int, Int)
counts xs = counts' xs (0, 0, 0, 0)
  where counts' [] t = t
        counts' (x:xs) (a, b, c, d)
          | x >= 0    && x <= 0.25 = counts' xs (a + 1, b    , c    , d    )
          | x >  0.25 && x <= 0.5  = counts' xs (a    , b + 1, c    , d    )
          | x >  0.5  && x <= 0.75 = counts' xs (a    , b    , c + 1, d    )
          | x >  0.75 && x <= 1    = counts' xs (a    , b    , c    , d + 1)

-- We can use the chi-squared test to check if the distribution is uniform.
-- The null hypothesis is that the result of probs is random and the
-- counts of the quartiles are equal.
-- The number of degrees of freedom is 3.
-- The chi-squared statistic (result of doChiSq) is 0.30245602
-- The critical value at a 90% significance level is 6.251
-- Critical value taken from NIST Engineering Statistics Handbook (2)
-- The critical value is higher than the chi-squared statistic and thus we can
-- not say that the results of probs are not random.

-- Time: 5 hours

-- (1) Software Testing Lab 2
-- (2) Critical Values of the Chi-Square Distribution. Retrieved September 11th 2017 from http://www.itl.nist.gov/div898/handbook/eda/section3/eda3674.htm
