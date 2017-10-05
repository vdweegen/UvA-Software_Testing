module Lab5 where

import Lecture5
import Example
import Data.List


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
-- Exercise 1 :: Time spent: 4+ hours
-- Mainly a direct copy from Lecture 5, however renamed the functions with a NRC
-- suffix. Furthermore introduced a now set of blocks, the blocksNRC. The most
-- challenging bit was figuring out that I had to use sameblock and sameblockNRC
-- in the prune function. So we check for the sameblock on the normal Soduku,
-- and also on the NRC blocks.
-- =============================================================================
exercise1 = solveAndShowNRC example

blocksNRC :: [[Int]]
blocksNRC = [[2..4],[6..8]]

blNRC :: Int -> [Int]
blNRC x = concat $ filter (elem x) blocksNRC

subGridNRC :: Sudoku -> (Row,Column) -> [Value]
subGridNRC s (r,c) =
  [ s (r',c') | r' <- blNRC r, c' <- blNRC c ]

freeInSubgridNRC :: Sudoku -> (Row,Column) -> [Value]
freeInSubgridNRC s (r,c) = freeInSeq (subGridNRC s (r,c))

freeAtPosNRC :: Sudoku -> (Row,Column) -> [Value]
freeAtPosNRC s (r,c) =
  (freeInRow s r)
   `intersect` (freeInColumn s c)
   `intersect` (freeInSubgrid s (r,c))
   `intersect` (freeInSubgridNRC s (r,c))

constraintsNRC :: Sudoku -> [Constraint]
constraintsNRC s = sortBy length3rd
    [(r,c, freeAtPosNRC s (r,c)) |
                       (r,c) <- openPositions s ]

consistentNRC :: Sudoku -> Bool
consistentNRC s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) |
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ subgridInjectiveNRC s (r,c) |
                    r <- [2,6], c <- [2,6]]

subgridInjectiveNRC :: Sudoku -> (Row,Column) -> Bool
subgridInjectiveNRC s (r,c) = injective vs where
   vs = filter (/= 0) (subGridNRC s (r,c))

initNodeNRC :: Grid -> [Node]
initNodeNRC gr = let s = grid2sud gr in
              if (not . consistentNRC) s then []
              else [(s, constraintsNRC s)]

solveAndShowNRC :: Grid -> IO[()]
solveAndShowNRC gr = solveShowNsNRC (initNodeNRC gr)

solveShowNsNRC :: [Node] -> IO[()]
solveShowNsNRC = sequence . fmap showNode . solveNsNRC

solveNsNRC :: [Node] -> [Node]
solveNsNRC = search succNodeNRC solved

succNodeNRC :: Node -> [Node]
succNodeNRC (s,[]) = []
succNodeNRC (s,p:ps) = extendNodeNRC (s,ps) p

extendNodeNRC :: Node -> Constraint -> [Node]
extendNodeNRC (s,constraintsNRC) (r,c,vs) =
   [(extend s ((r,c),v),
     sortBy length3rd $
         pruneNRC (r,c,v) constraintsNRC) | v <- vs ]

-- | Check for same block with and without NRC
pruneNRC :: (Row,Column,Value)
      -> [Constraint] -> [Constraint]
pruneNRC _ [] = []
pruneNRC (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : pruneNRC (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : pruneNRC (r,c,v) rest
  | sameblock (r,c) (x,y) = (x,y,zs\\[v]) : pruneNRC (r,c,v) rest
  | sameblockNRC (r,c) (x,y) =
        (x,y,zs\\[v]) : pruneNRC (r,c,v) rest
  | otherwise = (x,y,zs) : pruneNRC (r,c,v) rest

sameblockNRC :: (Row,Column) -> (Row,Column) -> Bool
sameblockNRC (r,c) (x,y) = blNRC r == blNRC x && blNRC c == blNRC y

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
