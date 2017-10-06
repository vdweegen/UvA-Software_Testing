module Lab5 where

import System.Random
import Data.List
import Data.Maybe (fromJust)
import Data.Sequence (Seq,fromList)
import Lecture5
import Example
import Control.Applicative

-- Define Main --
main = do
    putStrLn "===================="
    putStrLn "Assignment 5 / Lab 5"
    putStrLn "===================="
    putStrLn "> Exercise 1"
    exercise1
    putStrLn "> Exercise 2"
    -- exercise2
    putStrLn "> Exercise 3"
    exercise3
    putStrLn "> Exercise 4"
    exercise4
    putStrLn "> Exercise 5"
    exercise5
    putStrLn "> Exercise 6"
    -- exercise6
    putStrLn "> Exercise 7"
    -- exercise7

-- =============================================================================
-- Exercise 1 :: Time spent: +- 180 minutes
--
-- Basically modified the provided code to take the additional constraints into
-- account.
-- =============================================================================

exercise1 = do
  -- Not neccesarily a efficient solution, but since we work together sharing
  -- all the same base files (i.e. Example.hs and Lecture5.hs) I needed to find
  -- a solution that didn't require me modifying Lecture5.hs (which would have
  -- been alot easier)
  nrcSolveAndShow example

nrcBlock :: [[Int]]
nrcBlock = [[2..4],[6..8]]

nrcBl :: Int -> [Int]
nrcBl x = concat $ filter (elem x) nrcBlock

subBlock :: Sudoku -> (Row,Column) -> [Value]
subBlock s (r, c) = [ s (r',c')| r' <- nrcBl r ,c' <- nrcBl c]

nrcFreeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
nrcFreeInSubgrid s (r,c) = freeInSeq (subBlock s (r,c))

nrcFree :: Sudoku -> (Row,Column) -> [Value]
nrcFree s (r, c) =
  if r `elem` (concat nrcBlock) && c `elem` (concat nrcBlock) then
    (nrcFreeInSubgrid s (r,c))
  else values

nrcGridInjective :: Sudoku -> (Row,Column) -> Bool
nrcGridInjective s (r,c) = injective vs where
   vs = filter (/= 0) (subBlock s (r,c))

nrcFreeAtPos :: Sudoku -> (Row,Column) -> [Value]
nrcFreeAtPos s (r,c) = (freeInRow s r)
      `intersect` (freeInColumn s c)
      `intersect` (freeInSubgrid s (r,c))
      `intersect` (nrcFree s (r,c))

nrcConstraints :: Sudoku -> [Constraint]
nrcConstraints s = sortBy length3rd
    [(r,c, nrcFreeAtPos s (r,c)) |
     (r,c) <- openPositions s ]

nrcConsistent :: Sudoku -> Bool
nrcConsistent s = and $
  [ rowInjective s r |  r <- positions ]
  ++
  [ colInjective s c |  c <- positions ]
  ++
  [ subgridInjective s (r,c) | r <- [1,4,7], c <- [1,4,7]]
  ++
  [ nrcGridInjective s (r,c) | r <- [2, 6], c <- [2, 6]]

nrcInitNode :: Grid -> [Node]
nrcInitNode gr = let s = grid2sud gr in
                if (not . nrcConsistent) s then []
                else [(s, nrcConstraints s)]

nrcSolveNs :: [Node] -> [Node]
nrcSolveNs = search nrcSuccNode solved

nrcRsolveNs :: [Node] -> IO [Node]
nrcRsolveNs ns = rsearch nrcRsuccNode solved (return ns)

nrcSuccNode :: Node -> [Node]
nrcSuccNode (s,[]) = []
nrcSuccNode (s,p:ps) = nrcExtendNode (s,ps) p

nrcRsuccNode :: Node -> IO [Node]
nrcRsuccNode (s,cs) = do
  xs <- getRandomCnstr cs
  if null xs
    then
      return []
    else
      return (nrcExtendNode (s,cs\\xs) (head xs))

nrcExtendNode :: Node -> Constraint -> [Node]
nrcExtendNode (s,nrcConstraints) (r,c,vs) =
  [(extend s ((r,c),v), sortBy length3rd $ nrcPrune (r,c,v) nrcConstraints) | v <- vs ]

nrcPrune :: (Row,Column,Value) -> [Constraint] -> [Constraint]
nrcPrune _ [] = []
nrcPrune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : nrcPrune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : nrcPrune (r,c,v) rest
  | sameblock (r,c) (x,y) = (x,y,zs\\[v]) : nrcPrune (r,c,v) rest
  | nrcSameblock (r,c) (x,y) = (x,y,zs\\[v]) : nrcPrune (r,c,v) rest
  | otherwise = (x,y,zs) : nrcPrune (r,c,v) rest

nrcSameblock :: (Row, Column) -> (Row,Column) -> Bool
nrcSameblock (r,c) (x,y) = nrcBl r == nrcBl x && nrcBl c == nrcBl y

nrcSolveShowNs :: [Node] -> IO[()]
nrcSolveShowNs = sequence . fmap showNode . nrcSolveNs

nrcSolveAndShow :: Grid -> IO[()]
nrcSolveAndShow gr = nrcSolveShowNs (nrcInitNode gr)

--- SOLUTION
-- +-------+-------+-------+
-- | 4 7 8 | 3 9 2 | 6 1 5 |
-- | 6 1 9 | 7 5 8 | 3 2 4 |
-- | 2 3 5 | 4 1 6 | 9 7 8 |
-- +-------+-------+-------+
-- | 7 2 6 | 8 3 5 | 1 4 9 |
-- | 8 9 1 | 6 2 4 | 7 5 3 |
-- | 3 5 4 | 9 7 1 | 2 8 6 |
-- +-------+-------+-------+
-- | 5 6 7 | 2 8 9 | 4 3 1 |
-- | 9 8 3 | 1 4 7 | 5 6 2 |
-- | 1 4 2 | 5 6 3 | 8 9 7 |
-- +-------+-------+-------+

-- =============================================================================
-- Exercise 2 :: Time spent: +-
-- =============================================================================
exercise2 = do
  print()

type Position = (Row,Column)
type Constrnt = [[Position]]

rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let
  ys = filter (elem (r,c)) xs
  in
  foldl1 intersect (map ((values \\) . map s) ys)

-- =============================================================================
-- Exercise 3 :: Time spent: +- 480 minutes
--
-- Added code that will randomly change coordinates in a grid to 0, modified
-- the testcode from Lab2 and run the 'null-coordinates' function x times. If
-- a Sudoku is found that ALSO has 1 solution, and it is NOT the same Sudoku as
-- the original (i.e. a 0 has been changed to 0) than the Sudoku is not minimal
-- =============================================================================
exercise3 = do
  test 1 100 example testGrid

test :: Integer -> Integer -> Grid -> (Grid -> IO Grid) -> IO ()
test k n i f =
  if k == n then
    print (show n ++ " tests passed")
  else do
    x <- f i
    if (nrcSolveAndCount x) > 1 || (x == i) then
      do test (k+1) n i f
    else print ("failed after " ++ (show k) ++ " attempts. " ++ show (nrcSolveAndCount x) ++ " solutions." ++ show x)

testGrid :: Grid -> IO Grid
testGrid gr = do
  x <- randomRow gr
  n <- randomRIO (0, length gr -1)
  return $ replaceAtIndex (findRowIndex x gr) (replaceNth n x) gr

replaceAtIndex :: Int -> [Int] -> [[Int]] -> [[Int]]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

replaceNth :: Int -> [Int] -> [Int]
replaceNth n (x:xs)
  | n == 0 = 0:xs
  | otherwise = x:replaceNth (n-1) xs

randomRow :: Grid -> IO [Int]
randomRow gr = let
  x = pick gr
  in x

findRowIndex :: [Int] -> [[Int]] -> Int
findRowIndex r gr = fromJust $ elemIndex r gr

mapOnce :: (a -> Maybe a) -> [a] -> [a]
mapOnce _ []     = []
mapOnce f (x:xs) = case f x of
  Nothing -> x : mapOnce f xs
  Just y  -> y : xs

removeHint :: [Int] -> [Int]
removeHint r = mapOnce check r where
  check c | c /= 0 = Just 0
          | otherwise = Nothing

calculateHints :: Grid -> Int
calculateHints gr = sum $ map calculateHintsRow gr

calculateHintsRow :: [Value] -> Int
calculateHintsRow r = sum $ map (\a -> 1) $ filter (> 1) r

nrcSolveAndCount :: Grid -> Int
nrcSolveAndCount gr =
  let
    x = nrcSolveNs (nrcInitNode gr)
  in length x

-- =============================================================================
-- Exercise 4 :: Time spent: +- 180 minutes
--
-- Chose the following approach:
-- 1. define all blocks (by combing the blocks, row, and colums)
-- 2. calculate the powerset (all subsets, including itself and empty)
-- 3. filter the number of sets, if we choose x, then the number of blocks
--    that are ignored is 9 - x
-- 4. use modified version of genProblem (that takes a grid as argument) to
--    calculate the posibilities given that the other blocks are already fileld
--    (i.e. they are ignore and can be left empty)
-- =============================================================================
exercise4 = do
  presetEmptyProblems emptyblocks2
  presetEmptyProblems emptyblocks3
  presetEmptyProblems emptyblocks4
  presetEmptyProblems emptyblocks5

-- Block definitions
emptyblocks2 = filter(\x -> length x == 7) (powerset blocksALL) -- lenght is 6, 3 blocks are empty
emptyblocks3 = filter(\x -> length x == 6) (powerset blocksALL) -- lenght is 6, 3 blocks are empty
emptyblocks4 = filter(\x -> length x == 5) (powerset blocksALL) -- lenght is 5, 4 blocks are empty
emptyblocks5 = filter(\x -> length x == 4) (powerset blocksALL) -- lenght is 4, 5 blocks are empty

-- Walk through the possibilities using presetEmptyProblem
presetEmptyProblems :: [[[(Row,Column)]]] -> IO ()
presetEmptyProblems [] = print $ "done"
presetEmptyProblems (x:xs) = do
  presetEmptyProblem (concat x)
  presetEmptyProblems xs

-- Generate a single problem with a preset
presetEmptyProblem :: [(Row,Column)] -> IO ()
presetEmptyProblem gr = do
  [r] <- rsolveNs [emptyN]
  showNode r
  s  <- presetGenProblem r gr
  showNode s

-- modified genproblem to specify a node with filled coordinates
presetGenProblem :: Node -> [(Row, Column)] -> IO Node
presetGenProblem n gr = do
  ys <- randomize xs
  return (minimalize n ys)
  where xs = gr

-- Fancy combine function
combine :: [Row] -> [Column] -> [(Row,Column)]
combine x y = liftA2 (,) x y

-- Rows of 3 blocks
blocksT,blocksM,blocksB :: [Int]
blocksT = blocks !! 0
blocksM = blocks !! 1
blocksB = blocks !! 2

-- Individual blocks
blockTT,blockTM,blockTB :: [(Row,Column)]
blockMT,blockMM,blockMB :: [(Row,Column)]
blockBT,blockBM,blockBB :: [(Row,Column)]
blockTT = combine blocksT blocksT
blockTM = combine blocksT blocksM
blockTB = combine blocksT blocksB
blockMT = combine blocksM blocksT
blockMM = combine blocksM blocksM
blockMB = combine blocksM blocksB
blockBT = combine blocksB blocksT
blockBM = combine blocksB blocksM
blockBB = combine blocksB blocksB

-- List of all blocks
blocksALL :: [[(Row,Column)]]
blocksALL = [blockTT,blockTM,blockTB,
             blockMT,blockMM,blockMB,
             blockBT,blockBM,blockBB]

-- Powerset definition (borrowed from haskell.org)
powerset       :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

-- Modified version to count
solveAndCount :: Grid -> Int
solveAndCount gr =
  let
    x = solveNs (initNode gr)
  in length x

-- Used this to prove it was actually possible
-- emptyblocks4 :: Grid
-- emptyblocks4 = [[1,3,4,0,0,0,0,0,0],
--                 [8,6,5,0,0,0,0,0,0],
--                 [2,7,9,0,0,0,0,0,0],
--                 [5,2,6,3,4,7,0,0,0],
--                 [7,9,1,8,2,6,0,0,0],
--                 [3,4,8,5,9,1,2,0,0],
--                 [4,5,7,0,0,0,6,3,1],
--                 [6,8,3,0,0,0,9,7,2],
--                 [9,1,2,0,0,0,8,5,4]]

-- =============================================================================
-- Exercise 5 :: Time spent: +- 150 minutes
--
-- Basically modified the provided generator to take the additional constraints
-- into account
-- =============================================================================
exercise5 = do
  [r] <- nrcRsolveNs [emptyN]
  showNode r
  s  <- nrcGenProblem r
  showNode s

nrcEraseN :: Node -> (Row,Column) -> Node
nrcEraseN n (r,c) = (s, nrcConstraints s)
  where s = eraseS (fst n) (r,c)

nrcUniqueSol :: Node -> Bool
nrcUniqueSol node = singleton (nrcSolveNs [node]) where
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

nrcMinimalize :: Node -> [(Row,Column)] -> Node
nrcMinimalize n [] = n
nrcMinimalize n ((r,c):rcs) | nrcUniqueSol n' = nrcMinimalize n' rcs
                            | otherwise    = nrcMinimalize n  rcs
  where n' = nrcEraseN n (r,c)

nrcGenProblem :: Node -> IO Node
nrcGenProblem n = do
  ys <- randomize xs
  return (nrcMinimalize n ys)
  where xs = filledPositions (fst n)

-- =============================================================================
-- =============================================================================
-- Exercise 6 :: Time spent: +-
exercise6 = do
  print()

-- =============================================================================
-- Exercise 7 :: Time spent: +-
-- =============================================================================
exercise7 = do
  print()
