module Lab2 where
 
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

--Triangle Exercise

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle:: Int -> Int -> Int -> Shape
triangle c1 c2 h 
        |((c1 + c2) < h) || ((c1 + h) < c2) || ((h + c2) < c1) ||  (c1 <= 0) || (c2 <= 0) || (h <= 0) = NoTriangle
        |otherwise = whatKind c1 c2 h

whatKind:: Int -> Int -> Int -> Shape
whatKind c1 c2 h
        |(c1 == c2) && (c1 == h) = Equilateral
        |(c1 == c2) || (c1 == h) || (h == c2) = Isosceles
        |((c1^2)+(c2^2) == (h^2)) || ((c1^2)+(h^2) == (c2^2)) || ((h^2)+(c2^2) == (c1^2)) = Rectangular
        |otherwise = Other

{--
quickCheck(\x y z -> (x >= y+z || y > x+z || z > x+y || x <= 0 || y <= 0 || z <= 0) --> triangle x y z == NoTriangle)

quickCheck (\x y z -> (x == y && y == z) --> triangle x y z == Equilateral)

quickCheck (\x y z -> (c1 == c2 || c1 == h || h == c2) --> triangle x y z == Isosceles)

quickCheck (\x y z -> ((c1^2)+(c2^2) == (h^2)) || ((c1^2)+(h^2) == (c2^2)) || ((h^2)+(c2^2) == (c1^2)) --> triangle x y z == Rectangular)

quickCheck(\x y z -> ((x < y+z || y < x+z || z < x+y) && (x > 0 && y > 0 && z > 0) && not(x == y && y == z) && not((x == y) || (x == z) || (y == z) && not(((x^2)+(y^2) == (z^2)) || ((x^2)+(z^2) == (y^2)) || ((z^2)+(y^2) == (x^2)))) --> triangle x y z == Other)


--}

--Time spent: 2 hours

