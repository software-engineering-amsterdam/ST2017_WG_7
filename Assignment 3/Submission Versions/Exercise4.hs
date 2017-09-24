module Exercise4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Exercise1
import Exercise3

-- Exercise 4 - Erik van Scharrenburg
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


cnfTest :: Form -> Bool
cnfTest f = prop_equiv f cnff
         && prop_cnf cnff
  where cnff = (cnf . nnf . arrowfree) f

prop_equiv :: Form -> Form -> Bool
prop_equiv a b = a `equiv` b

prop_cnf :: Form -> Bool
prop_cnf (Cnj fs) = all clause fs
prop_cnf (Dsj fs) = all literal fs
prop_cnf f = literal f

clause :: Form -> Bool
clause (Dsj fs) = all literal fs
clause f = literal f

literal :: Form -> Bool
literal (Prop _) = True
literal (Neg (Prop _)) = True
literal _ = False

-- 'quickCheck cnfTest' will check that a random formula f is equivalent to
-- cnf f and that cnf f is in conjunctive normal form.

-- Time: 1 hour and 30 minutes
