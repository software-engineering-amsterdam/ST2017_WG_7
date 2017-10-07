module Lab5_Ex4 where

import Data.List
import System.Random
import Lecture5


posFromIO:: IO [(Row,Column)] -> [(Row,Column)]
posFromIO = posFromIO

blocksPos:: [(Row,Column)]
blocksPos = [(1,1),(1,4),(1,7),(4,1),(4,4),(4,7),(7,1),(7,4),(7,7)]

eraseGrid:: Sudoku -> [(Row,Column)] -> Sudoku
eraseGrid s [] = s
eraseGrid s (x:xs) = eraseGrid (eraseS s x) xs

subGridPos :: (Row,Column) -> [(Row,Column)]
subGridPos (r,c) = 
  [ (r',c') | r' <- bl r, c' <- bl c ]


--To do the function gridsToEmpty I used the function from this website
--https://www.rosettacode.org/wiki/Pick_random_element#Haskell

gridsToEmpty:: [(Row,Column)] -> IO [(Row,Column)]
gridsToEmpty xs = fmap (list !!) $ randomRIO (0, length list - 1)
                  where list = filter (\x -> (length x == 3)) (subsequences xs)


posToEmpty::[(Row,Column)] -> [(Row,Column)]
posToEmpty [] = []
posToEmpty (x:xs) = (subGridPos x) ++ (posToEmpty xs)

result:: Sudoku -> IO [Sudoku]
result s = do
             x <- gridsToEmpty blocksPos
             return [(eraseGrid s (posToEmpty x))]

generateSudoku = do 
                   [n] <- rsolveNs [emptyN]
                   (s,_) <- genProblem n
                   [y] <- result s
                   showSudoku y

{--
*Lab5_Ex4> generateSudoku
+-------+-------+-------+
| 9     |       | 1 8   |
|     7 |     5 |       |
|   5   | 1   4 |       |
+-------+-------+-------+
|       | 9     |       |
|       |       |       |
|       |       |       |
+-------+-------+-------+
|       | 5   1 | 3 6   |
|       | 2     |     8 |
|       |   3   |     9 |
+-------+-------+-------+
--}

--Time spent: 5 hours








