module Lab5 where
import qualified Lab5.Jordan.Lecture5Original as Lec5 (solveAndShow, Grid, rsolveNs, emptyN, showNode, genProblem, filledPositions, randomize, eraseN, uniqueSol, Sudoku, Row, Column, Node)
import qualified Lab5.Jordan.Lecture5 as NRC (solveAndShow, Grid, rsolveNs, emptyN, showNode, genProblem, filledPositions)
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


example :: NRC.Grid
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

exercise1 = do
  NRC.solveAndShow example
  
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
-- Exercise 3 :: Time spent: +- 5 hours
-- =============================================================================
exercise3 = do
  putStrLn "Are the generated sudokus minimal?"
  xs <-  (checkerMulti 5)
  let x = and xs
  print x

  {-- 
  This article helped me understand the problem 
  https://www.technologyreview.com/s/426554/mathematicians-solve-minimum-sudoku-problem/
  --}

checker' :: IO Bool
checker' = do
  [r] <- Lec5.rsolveNs [Lec5.emptyN]
  s  <- Lec5.genProblem r
  x <- Lec5.randomize ( Lec5.filledPositions (fst s))
  s' <- do
    return (Lec5.eraseN s (head x))
  return $ not $ Lec5.uniqueSol s'

checker :: IO Bool
checker = do
  [r] <- Lec5.rsolveNs [Lec5.emptyN]
  s  <- Lec5.genProblem r
  let x  = (Lec5.filledPositions (fst s))
  let s' = map (eraseNcheck s) x
  return $ and s'
  
eraseNcheck :: Lec5.Node -> (Lec5.Row, Lec5.Column) ->  Bool
eraseNcheck s pos = not $ Lec5.uniqueSol (Lec5.eraseN s pos)

checkerMulti :: Int -> IO [Bool]
checkerMulti n = do
  replicateM n $ checker
 
  

-- =============================================================================
-- Exercise 4 :: Time spent: +- 3 hours
-- =============================================================================
{-- 

Yes you can do 3 you can even do 4 but not shure if you can do more. Rhymes
This will generate a minimal sudoku with 3/4 blocks empty. There are some situations
where the blocks are cannot removed because it will cause the problem to be ambiguous
If this happens the functions tries again until it finds a problem that only has 1 unique
solution.
--}
exercise4 = print()
-- exercise4 = do
--   checkerBlocksMulti 1

-- checkerBlocks :: IO ()
-- checkerBlocks = do
--   z <- takeM
--   showNode z

-- minimizebyBlock ::  Node -> [(Row,Column)] -> Int -> (Node, Int)
-- minimizebyBlock n [] steps = (n,steps)
-- minimizebyBlock n ((r,c):rcs) steps | uniqueSol n' = minimizebyBlock n' rcs (steps+1)
--                                     | otherwise    = minimizebyBlock n  rcs (steps)
--       where n' = eraseN n (r,c)

-- minimizeUntil = do 
--   z <- takeM
--   showNode z

-- takeM = do 
--   [n] <- rsolveNs [emptyN]
--   ys <-  do4blocks
--   let (n', steps) = (minimizebyBlock n ys 0)
--   if steps == (genericLength ys) then 
--     return n'
--   else 
--     takeM
  
-- do3blocks = do
--   let xs = blockConstrnt
--   first <- pick ((\\) blockConstrnt [])
--   second <- pick ((\\) blockConstrnt [first])
--   third <- pick ((\\) blockConstrnt [first, second])
--   return $ first ++ second ++ third

-- do4blocks = do
--   let xs = blockConstrnt
--   first <- pick ((\\) blockConstrnt [])
--   second <- pick ((\\) blockConstrnt [first])
--   third <- pick ((\\) blockConstrnt [first, second])
--   fourth <- pick ((\\) blockConstrnt [first, second, third])
--   return $ first ++ second ++ third ++ fourth
  


-- checkerBlocksMulti n = do
--   replicateM n $ checkerBlocks
   

-- =============================================================================
-- Exercise 5 :: Time spent: +- 2 hours
-- =============================================================================
exercise5 = do
  print()

-- =============================================================================
-- Exercise 6 :: Time spent: +-
-- =============================================================================
exercise6 = do

  
  print()

-- =============================================================================
-- Exercise 7 :: Time spent: +- 1 hour
-- =============================================================================
exercise7 = do
  runTestAvgHints 5


runTestAvgHints n = do
  x <- replicateM n generateAndCountLec
  y <- replicateM n generateAndCountNRC
  putStrLn "Average number of hints"
  let xAvg =  (fromIntegral (sum x)) / (fromIntegral (genericLength x))
  print xAvg
  putStrLn "Average NRC of hints"
  let yAvg =  (fromIntegral (sum y)) / (fromIntegral (genericLength y))
  print yAvg

generateAndCountLec :: IO Int
generateAndCountLec = do
  [n] <- Lec5.rsolveNs [Lec5.emptyN]
  p <- Lec5.genProblem n
  return $ genericLength $  Lec5.filledPositions (fst p)

generateAndCountNRC :: IO Int
generateAndCountNRC = do
  [n] <- NRC.rsolveNs [NRC.emptyN]
  p <- NRC.genProblem n
  return $ genericLength$  NRC.filledPositions (fst p)
