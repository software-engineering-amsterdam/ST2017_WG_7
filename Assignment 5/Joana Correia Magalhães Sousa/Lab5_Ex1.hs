module Lab5_Ex1 where
 
import Data.List
import System.Random
--import Lecture5
              

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

nrcblocks :: [[Int]]
nrcblocks = [[2..4],[6..8]]

showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")

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

nrcbl :: Int -> [Int]
nrcbl x = concat $ filter (elem x) nrcblocks 

subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = 
  [ s (r',c') | r' <- bl r, c' <- bl c ]

subNRCGrid :: Sudoku -> (Row,Column) -> [Value]
subNRCGrid s (r,c) = 
  [ s (r',c') | r' <- nrcbl r, c' <- nrcbl c ]

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

freeInSubNRCgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubNRCgrid s (r,c) = freeInSeq (subNRCGrid s (r,c))

freeAtPosNRC :: Sudoku -> (Row,Column) -> [Value]
freeAtPosNRC s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c))
   `intersect` (freeInSubNRCgrid s (r,c))

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

subNRCgridInjective :: Sudoku -> (Row,Column) -> Bool
subNRCgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subNRCGrid s (r,c))

consistentNRC :: Sudoku -> Bool
consistentNRC s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ subNRCgridInjective s (r,c) |
                    r <- [2,6], c <- [2,6]]

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
-----------------------------------------------------------------------
extendNodeNRC :: Node -> Constraint -> [Node]
extendNodeNRC (s,constraintsNRC) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         pruneNRC (r,c,v) constraintsNRC) | v <- vs ]

pruneNRC :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
pruneNRC _ [] = []
pruneNRC (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : pruneNRC (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : pruneNRC (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : pruneNRC (r,c,v) rest
  | samenrcblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : pruneNRC (r,c,v) rest
  | otherwise = (x,y,zs) : pruneNRC (r,c,v) rest

sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y

samenrcblock :: (Row,Column) -> (Row,Column) -> Bool
samenrcblock (r,c) (x,y) = nrcbl r == nrcbl x && nrcbl c == nrcbl y 

initNodeNRC :: Grid -> [Node]
initNodeNRC gr = let s = grid2sud gr in 
              if (not . consistentNRC) s then [] 
              else [(s, constraintsNRC s)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

constraintsNRC :: Sudoku -> [Constraint] 
constraintsNRC s = sortBy length3rd 
    [(r,c, freeAtPosNRC s (r,c)) | 
                       (r,c) <- openPositions s ]
-----------------------------------------------------------------------
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

solveNsNRC :: [Node] -> [Node]
solveNsNRC = search succNodeNRC solved 

succNodeNRC :: Node -> [Node]
succNodeNRC (s,[]) = []
succNodeNRC (s,p:ps) = extendNodeNRC (s,ps) p 

solveAndShowNRC :: Grid -> IO[()]
solveAndShowNRC gr = solveShowNsNRC (initNodeNRC gr)

solveShowNsNRC :: [Node] -> IO[()]
solveShowNsNRC = sequence . fmap showNode . solveNsNRC


exampleNRC::Grid
exampleNRC = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]]


{--
*Lab5_Ex1> solveAndShowNRC exampleNRC
+-------+-------+-------+
| 4 7 8 | 3 9 2 | 6 1 5 |
| 6 1 9 | 7 5 8 | 3 2 4 |
| 2 3 5 | 4 1 6 | 9 7 8 |
+-------+-------+-------+
| 7 2 6 | 8 3 5 | 1 4 9 |
| 8 9 1 | 6 2 4 | 7 5 3 |
| 3 5 4 | 9 7 1 | 2 8 6 |
+-------+-------+-------+
| 5 6 7 | 2 8 9 | 4 3 1 |
| 9 8 3 | 1 4 7 | 5 6 2 |
| 1 4 2 | 5 6 3 | 8 9 7 |
+-------+-------+-------+
[()]
*Lab5_Ex1> 

--}

--Time spent: 3 hours










