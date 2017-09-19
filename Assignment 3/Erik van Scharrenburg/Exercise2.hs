module Exercise2 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Exercise 2
instance Arbitrary Form where
  arbitrary = sized genForm

genForm :: Int -> Gen Form
genForm 0 = genProp
genForm 1 = genProp
genForm 2 = genProp
genForm i = oneof [genNeg   (i - 1),
                   genCnj   (i - 1),
                   genDsj   (i - 1),
                   genImpl  (i - 1),
                   genEquiv (i - 1)]

genProp :: Gen Form
genProp = do p <- choose (0, 1000)
             return (Prop p)

genNeg :: Int -> Gen Form
genNeg i = do f <- genForm i
              return (Neg f)

genCnj :: Int -> Gen Form
genCnj i = do n <- choose(2, i)
              f <- vectorOf n (genForm (i `div` n))
              return (Cnj f)

genDsj :: Int -> Gen Form
genDsj i = do n <- choose(2, i)
              f <- vectorOf n (genForm (i `div` n))
              return (Dsj f)

genImpl :: Int -> Gen Form
genImpl i = do f1 <- genForm (i `div` 2)
               f2 <- genForm (i `div` 2)
               return (Impl f1 f2)

genEquiv :: Int -> Gen Form
genEquiv i = do f1 <- genForm (i `div` 2)
                f2 <- genForm (i `div` 2)
                return (Equiv f1 f2)

parseTest :: IO ()
parseTest = quickCheck p
  where p :: Form -> Bool
        p f = [f] == parse (show f)

-- The test generates forms and uses to show funtion to convert them to a
-- string. This string is parsed and compared to the original form.
-- For the test it is assumed that the Show Form instance is correct.

-- Time: 1 hour and 30 minutes
