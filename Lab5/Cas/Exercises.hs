module Lab5 where

import Data.List
import Lecture5
import Example

-- Define Main --
main = do
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

-- =============================================================================
-- Exercise 1 :: Time spent: +-
-- =============================================================================

exercise1 = do
  -- Not neccesarily a efficient solution, but since we work together sharing
  -- all the same base files (i.e. Example.hs and Lecture5.hs) I needed to find
  -- a solution that didn't require me modifying Lecture5.hs (which would have
  -- been alot easier)
  solveAndShowNrc example

nrcBlock :: [[Int]]
nrcBlock = [[2..4],[6..8]]

nrcBl :: Int -> [Int]
nrcBl x = concat $ filter (elem x) nrcBlock

subBlock :: Sudoku -> (Row,Column) -> [Value]
subBlock s (r, c) = [ s (r',c')| r' <- nrcBl r ,c' <- nrcBl c]

freeInNrcgrid :: Sudoku -> (Row,Column) -> [Value]
freeInNrcgrid s (r,c) = freeInSeq (subBlock s (r,c))

freeNrc :: Sudoku -> (Row,Column) -> [Value]
freeNrc s (r, c) =
  if r `elem` (concat nrcBlock) && c `elem` (concat nrcBlock) then
    (freeInNrcgrid s (r,c))
  else values

nrcGridInjective :: Sudoku -> (Row,Column) -> Bool
nrcGridInjective s (r,c) = injective vs where
   vs = filter (/= 0) (subBlock s (r,c))

nrcFreeAtPos :: Sudoku -> (Row,Column) -> [Value]
nrcFreeAtPos s (r,c) = (freeInRow s r)
      `intersect` (freeInColumn s c)
      `intersect` (freeInSubgrid s (r,c))
      `intersect` (freeNrc s (r,c))

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

initNrcNode :: Grid -> [Node]
initNrcNode gr = let s = grid2sud gr in
                if (not . nrcConsistent) s then []
                else [(s, nrcConstraints s)]

nrcSolveNs :: [Node] -> [Node]
nrcSolveNs = search nrcSuccNode solved

nrcRsolveNs :: [Node] -> IO [Node]
nrcRsolveNs ns = rsearch rsuccNode solved (return ns)

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

solveAndShowNrc :: Grid -> IO[()]
solveAndShowNrc gr = nrcSolveShowNs (initNrcNode gr)

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

-- =============================================================================
-- Exercise 3 :: Time spent: +-
-- =============================================================================
exercise3 = do
  print()

-- =============================================================================
-- Exercise 4 :: Time spent: +-
-- =============================================================================
exercise4 = do
  print()

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
