module Lab5 where
import Lab5.Jordan.Lecture5
import Data.List
import Control.Monad
import System.Random
import System.Random (randomRIO)

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)


-- Define Main --
main' = do
    putStrLn "===================="
    putStrLn "Assignment 5 / Lab 5"
    putStrLn "===================="
    putStrLn "> Exercise 1"
    exercise1
    putStrLn "> Exercise 2"
    exercise2
    putStrLn "> Exercise 3"
    exercise3
    putStrLn "> Exercise 4"
    exercise4
    putStrLn "> Exercise 5"
    exercise5
    putStrLn "> Exercise 6"
    exercise6
    putStrLn "> Exercise 7"
    exercise7




example :: Grid
example = [[0,0,0,3,0,0,0,0,0],
            [0,0,0,7,0,0,3,0,0],
            [2,0,0,0,0,0,0,0,8],
            [0,0,6,0,0,5,0,0,0],
            [0,9,1,6,0,0,0,0,0],
            [3,0,0,0,7,1,2,0,0],
            [0,0,0,0,0,0,0,3,1],
            [0,8,0,0,4,0,0,0,0],
            [0,0,2,0,0,0,0,0,0]]




-- =============================================================================
-- Exercise 1 :: Time spent: +- 30 minutes
-- =============================================================================

exercise1 = solveAndShow example
{--

freeInNRCSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInNRCSubgrid s (r,c) = freeInSeq (getNrcGrid s (r,c))

freeAtPos' :: Sudoku -> (Row,Column) -> [Value]
freeAtPos' s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInNRCSubgrid s (r,c))
   `intersect` (freeInSubgrid s (r,c)) 

nrcGridInjective :: Sudoku -> (Row,Column) -> Bool
nrcGridInjective s (r,c) = injective vs where 
  vs = filter (/= 0) (getNrcGrid s (r,c))

bl' :: Int -> [Int]
bl' x = concat $ filter (elem x) blocks' 

blocks' :: [[Int]]
blocks' = [[2,3,4],[6,7,8]]

getNrcGrid :: Sudoku -> (Row,Column) -> [Value]
getNrcGrid s (r,c) = 
  [ s (r',c') | r' <- bl' r, c' <- bl' c ]
   
consistent :: Sudoku -> Bool
consistent s = and $
                [ rowInjective s r |  r <- positions ]
                ++
                [ colInjective s c |  c <- positions ]
                ++
                [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]
                ++
                [ nrcGridInjective s (r,c) | 
                r <- [2,6], c <- [2, 6]]

sameblock' :: (Row,Column) -> (Row,Column) -> Bool
sameblock' (r,c) (x,y) = bl' r == bl' x && bl' c == bl' y 

prune' :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune' _ [] = []
prune' (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | sameblock (r,c) (x,y) = 
    (x,y,zs\\[v]) : prune' (r,c,v) rest
  | sameblock' (r,c) (x,y) = 
      (x,y,zs\\[v]) : prune' (r,c,v) rest
  | otherwise = (x,y,zs) : prune' (r,c,v) rest

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


--}


-- =============================================================================
-- Exercise 2 :: Time spent: +- 1 hour
-- Check the lecture5
-- =============================================================================
exercise2 = do
  print()



-- freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
-- freeAtPos' s (r,c) xs = let 
--     ys = filter (elem (r,c)) xs 
--     in 
--     foldl1 intersect (map ((values \\) . map s) ys)
-- =============================================================================
-- Exercise 3 :: Time spent: +-
-- =============================================================================
exercise3 = do
  putStrLn "Are the generated sudokus minimal?"
  (checkerMulti 5)

  {-- 
  This article helped me understand the problem 
  https://www.technologyreview.com/s/426554/mathematicians-solve-minimum-sudoku-problem/
  --}

checker :: IO Bool
checker = do
  [r] <- rsolveNs [emptyN]
  -- showNode r
  s  <- genProblem r
  -- showNode s
  -- [r] <- rsolveNs [s]
  -- print $ uniqueSol s
  x <- randomize ( filledPositions (fst s))
  s' <- do
    return (eraseN s (head x))
  -- showNode s'
  -- putStrLn "Is the problem still unique?"
  return $ not $ uniqueSol s'


checkerMulti :: Int -> IO [Bool]
checkerMulti n = do
  replicateM n $ checker
 
  

-- =============================================================================
-- Exercise 4 :: Time spent: +- 3 hours
-- =============================================================================
{-- 

Yes you can do 3 you can even do 4 but not shure if you can do more. Rhymes

--}

exercise4 = do
  checkerBlocksMulti 1

checkerBlocks :: IO ()
checkerBlocks = do
  z <- takeM
  showNode z

minimizebyBlock ::  Node -> [(Row,Column)] -> Int -> (Node, Int)
minimizebyBlock n [] steps = (n,steps)
minimizebyBlock n ((r,c):rcs) steps | uniqueSol n' = minimizebyBlock n' rcs (steps+1)
                                    | otherwise    = minimizebyBlock n  rcs (steps)
      where n' = eraseN n (r,c)

minimizeUntil = do 
  z <- takeM
  showNode z

takeM = do 
  [n] <- rsolveNs [emptyN]
  ys <-  do4blocks
  let (n', steps) = (minimizebyBlock n ys 0)
  if steps == (genericLength ys) then 
    return n'
  else 
    takeM
  
do3blocks = do
  let xs = blockConstrnt
  first <- pick ((\\) blockConstrnt [])
  second <- pick ((\\) blockConstrnt [first])
  third <- pick ((\\) blockConstrnt [first, second])
  return $ first ++ second ++ third

do4blocks = do
  let xs = blockConstrnt
  first <- pick ((\\) blockConstrnt [])
  second <- pick ((\\) blockConstrnt [first])
  third <- pick ((\\) blockConstrnt [first, second])
  fourth <- pick ((\\) blockConstrnt [first, second, third])
  return $ first ++ second ++ third ++ fourth
  


checkerBlocksMulti n = do
  replicateM n $ checkerBlocks
   

-- =============================================================================
-- Exercise 5 :: Time spent: +-
-- =============================================================================
exercise5 = do
  print()

-- =============================================================================
-- Exercise 6 :: Time spent: +-
-- =============================================================================
exercise6 = do
  print()

-- =============================================================================
-- Exercise 7 :: Time spent: +-
-- =============================================================================
exercise7 = do
  print()
