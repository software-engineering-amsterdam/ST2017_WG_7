module Exercise2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Citation 1 start
data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)
-- Citation 1 end

-- Recognizing triangles
-- Exercise 2
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
  | noTriangle  a b c = NoTriangle
  | equilateral a b c = Equilateral
  | isosceles   a b c = Isosceles
  | rectangular a b c = Rectangular
  | otherwise = Other
  where
    -- No side should have zero length and the sum of two sides should be
    -- greater than the third side.
    noTriangle a b c = elem 0 [a, b, c]
                    || not ( a + b > c
                          || a + c > b
                          || b + c > a)
    -- All sides should have the same length
    equilateral a b c = a == b
                     && b == c
    -- Two sides should have the same length
    isosceles a b c = a == b
                   || a == c
                   || b == c
    -- As all lenths are integers they should be a Pythagorean triple
    rectangular a b c = a^2 + b^2 == c^2
                     || a^2 + c^2 == b^2
                     || b^2 + c^2 == a^2


triangleTest = forAll triangleTestListGen triangleTest'
  where triangleTest' (a, b, c) = Just (triangle a b c)
                               == lookup (a, b, c) triangleTestList

triangleTestListGen :: Gen (Integer, Integer, Integer)
triangleTestListGen = elements (map fst triangleTestList)

triangleTestList :: [((Integer, Integer, Integer), Shape)]
triangleTestList = [((  0,   0,   0), NoTriangle),
                    ((  1,   1,   1), Equilateral),
                    (( 10,  10, 100), Isosceles),
                    ((  3,   4,   5), Rectangular),
                    ((  1,   2,   3), Other),
                    (( 20,  20,   0), NoTriangle),
                    ((400, 400, 400), Equilateral),
                    (( 50,   2,  50), Isosceles),
                    (( 12,   9,  15), Rectangular),
                    (( 40,  30,  20), Other)
                    ]
-- I tested the correctness of the program by verifying it produces the
-- expected result for a random selection from a list where the correct result
-- is known.

-- Time: 45 minutes

-- (1) Software Testing Lab 2
