module Lab5_Ex5 where

import Data.List
import System.Random
import Lab5_Ex1

--This code imports the code from Lab5_ex1 since it is in this file 
-- that contains the exta constraints for the NRC Sudoku that are 
-- needed to create this generator

emptyN :: Node
emptyN = (\ _ -> 0,constraintsNRC (\ _ -> 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs 
                  if null y 
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs) 
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                      if null xs 
                        then return []
                        else return 
                          (extendNodeNRC (s,cs\\xs) (head xs))

rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node]) 
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = 
  do xs <- ionodes 
     if null xs 
       then return []
       else 
         if goal (head xs) 
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys 
                      then return [head ys]
                      else if null (tail xs) then return []
                           else 
                             rsearch 
                               succ goal (return $ tail xs)

genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

randomS = genRandomSudoku >>= showNode

uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNsNRC [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseS :: Sudoku -> (Row,Column) -> Sudoku
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)

eraseN :: Node -> (Row,Column) -> Node
eraseN n (r,c) = (s, constraintsNRC s) 
  where s = eraseS (fst n) (r,c) 

minimalize :: Node -> [(Row,Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                         | otherwise    = minimalize n  rcs
  where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = [ (r,c) | r <- positions,  
                              c <- positions, s (r,c) /= 0 ]

genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s

