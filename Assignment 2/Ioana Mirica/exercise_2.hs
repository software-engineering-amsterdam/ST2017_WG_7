module Lab2 where
import Data.List

--1
data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)
--1

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z = 
    --For it to be a triangle: all sides must be greater than 0 and the sum of any 2 sides must be greater than the 3rd side
    if (x <= 0 || y <= 0 || z <= 0) || (not ( x + y > z || x + z > y || y + z > x)) then NoTriangle else
    --For it to be a Equilateral triangle: all 3 sides must be equal
    if (x == y) && (y == z) then Equilateral else
    -- For it to be a Isosceles triangle: 2 sides must be equal
    if (x == y || x == z || y == z) then Isosceles else
    --For it to be a Rectangular triangle: we know that a Pitagora triangle is a Rectangular triangle, so we check if we have a Pythagorean triple (from the sides of the triangle) 
    if (x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || y^2 + z^2 == x^2) then Rectangular else Other


--DO THE TESTING PART

--Cite: 1 -> from Lab2 (+ Wikipedia reading for the conditions)
--Time: ~1h