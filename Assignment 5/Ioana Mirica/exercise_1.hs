module Lab5 where
import Data.List

-- exercise 1 -> Ioana Mirica
nrcSudoku :: Grid
nrcSudoku = [[0,0,0,3,0,0,0,0,0],
             [0,0,0,7,0,0,3,0,0],
             [2,0,0,0,0,0,0,0,8],
             [0,0,6,0,0,5,0,0,0],
             [0,9,1,6,0,0,0,0,0],
             [3,0,0,0,7,1,2,0,0],
             [0,0,0,0,0,0,0,3,1],
             [0,8,0,0,4,0,0,0,0],
             [0,0,2,0,0,0,0,0,0]]

showRow :: [Value] -> Int -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] n = 
 if n == 0 then do  putChar '|'         ; putChar ' '
                    putStr (showVal a1) ; putChar ' ' ; putChar ' ' ; putChar ' '
                    putStr (showVal a2) ; putChar ' '
                    putStr (showVal a3) ; putChar ' '
                    putChar '|'         ; putChar ' '
                    putStr (showVal a4) ; putChar ' ' ; putChar ' ' 
                    putStr (showVal a5) ; putChar ' ' ; putChar ' '
                    putStr (showVal a6) ; putChar ' '
                    putChar '|'         ; putChar ' '
                    putStr (showVal a7) ; putChar ' '
                    putStr (showVal a8) ; putChar ' ' ; putChar ' ' ; putChar ' '
                    putStr (showVal a9) ; putChar ' '
                    putChar '|'         ; putChar '\n'
 else           do  putChar '|'         ; putChar ' '
                    putStr (showVal a1) ; putChar ' '
                    putChar '|'         ; putChar ' '
                    putStr (showVal a2) ; putChar ' '
                    putStr (showVal a3) ; putChar ' '
                    putChar '|'         ; putChar ' '
                    putStr (showVal a4) ; 
                    putChar '|'         ; putChar ' '
                    putStr (showVal a5) ; putChar ' '
                    putChar '|'         ;
                    putStr (showVal a6) ; putChar ' '
                    putChar '|'         ; putChar ' '
                    putStr (showVal a7) ; putChar ' '
                    putStr (showVal a8) ; putChar ' '
                    putChar '|'         ; putChar ' '
                    putStr (showVal a9) ; putChar ' '
                    putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+---------+---------+---------+")
    showRow as 0
    putStrLn ("|   +--------+   +--------+   |")
    showRow bs 1; showRow cs 1
    putStrLn ("+---------+---------+---------+")
    showRow ds 1
    putStrLn ("|   +--------+   +--------+   |")
    showRow es 0
    putStrLn ("|   +--------+   +--------+   |")
    showRow fs 1
    putStrLn ("+---------+---------+---------+")
    showRow gs 1; showRow hs 1
    putStrLn ("|   +--------+   +--------+   |")
    showRow is 0
    putStrLn ("+---------+---------+---------+")

nrcBlocks :: [[Int]]
nrcBlocks = [[2..4],[6..8]]

nrcBl :: Int -> [Int]
nrcBl x = concat $ filter (elem x) nrcBlocks

nrcSubGrid :: Sudoku -> (Row,Column) -> [Value]
nrcSubGrid s (r,c) =
  [ s (r',c') | r' <- nrcBl r, c' <- nrcBl c ]
 
nrcFreeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
nrcFreeInSubgrid s (r,c) = freeInSeq (nrcSubGrid s (r,c))

nrcFreeAtPos :: Sudoku -> (Row,Column) -> [Value]
nrcFreeAtPos s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c))
   `intersect` (nrcFreeInSubgrid s (r,c))

nrcSubgridInjective :: Sudoku -> (Row,Column) -> Bool
nrcSubgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (nrcSubGrid s (r,c))

--Some of Lecture 5 code
type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

showVal :: Value -> String
showVal 0 = " "
showVal d = show d

type Sudoku = (Row,Column) -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks 

subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = 
  [ s (r',c') | r' <- bl r, c' <- bl c ]

freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq 

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = 
  freeInSeq [ s (r,i) | i <- positions  ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = 
  freeInSeq [ s (i,c) | i <- positions ]

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c)) 

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where 
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where 
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid s (r,c))

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]

extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x 

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune (r,c,v) constraints) | v <- vs ]

prune :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) = (x,y,zs\\[v]) : prune (r,c,v) rest
  | otherwise = (x,y,zs) : prune (r,c,v) rest

sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = (bl r == bl x && bl c == bl y) || (nrcBl r == nrcBl x && nrcBl c == nrcBl y) --stop the infinite run

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

constraints :: Sudoku -> [Constraint] 
constraints s = sortBy length3rd 
    [(r,c, nrcFreeAtPos s (r,c)) | 
                       (r,c) <- openPositions s ]

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]

grow :: (node -> [node]) -> node -> Tree node 

grow step seed = T seed (map (grow step) (step seed))

count :: Tree a -> Int 
count (T _ ts) = 1 + sum (map count ts)

takeT :: Int -> Tree a -> Tree a
takeT 0 (T x _) = T x []
takeT n (T x ts) = T x $ map (takeT (n-1)) ts

search :: (node -> [node]) 
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs) 
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved 

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p 

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs = sequence . fmap showNode . solveNs

--Solution

-- +---------+---------+---------+
-- | 4   7 8 | 3  9  2 | 6 1   5 |
-- |   +--------+   +--------+   |
-- | 6 | 1 9 | 7| 5 |8 | 3 2 | 4 |
-- | 2 | 3 5 | 4| 1 |6 | 9 7 | 8 |
-- +---------+---------+---------+
-- | 7 | 2 6 | 8| 3 |5 | 1 4 | 9 |
-- |   +--------+   +--------+   |
-- | 8   9 1 | 6  2  4 | 7 5   3 |
-- |   +--------+   +--------+   |
-- | 3 | 5 4 | 9| 7 |1 | 2 8 | 6 |
-- +---------+---------+---------+
-- | 5 | 6 7 | 2| 8 |9 | 4 3 | 1 |
-- | 9 | 8 3 | 1| 4 |7 | 5 6 | 2 |
-- |   +--------+   +--------+   |
-- | 1   4 2 | 5  6  3 | 8 9   7 |
-- +---------+---------+---------+

--Time: ~6h