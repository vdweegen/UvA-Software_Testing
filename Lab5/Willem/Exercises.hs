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
-- Exercise 2 :: Time spent: +- 1.5 hours
-- Added nrcConstrnt on how simple it is to add a new constraint
-- =============================================================================
exercise2 = solveAndShow' example

type Position = (Row,Column)
type Constrnt = [[Position]]

rowConstrnt, columnConstrnt, blockConstrnt, nrcConstrnt, allConstrnt :: Constrnt
rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

-- | Very easy to add some more constraints :)
nrcConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocksNRC, b2 <- blocksNRC ]

allConstrnt = rowConstrnt ++ columnConstrnt ++ blockConstrnt ++ nrcConstrnt

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let
   ys = filter (elem (r,c)) xs
 in
   foldl1 intersect (map ((values \\) . map s) ys)

constraints' :: Sudoku -> [Constraint]
constraints' s = sortBy length3rd
    [(r,c, freeAtPos' s (r,c) allConstrnt) |
                       (r,c) <- openPositions s ]

consistent' :: Sudoku -> Bool
consistent' s = all (constrntInjective s) allConstrnt

-- | Check if a constraint is injective instead of a function per row, column, grid etc
constrntInjective :: Sudoku -> [(Row,Column)] -> Bool
constrntInjective s xs = injective xs

emptyN' :: Node
emptyN' = (\ _ -> 0,constraints' (\ _ -> 0))

getRandomCnstr' :: [Constraint] -> IO [Constraint]
getRandomCnstr' cs = getRandomItem (f cs)
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode' :: Node -> IO [Node]
rsuccNode' (s,cs) = do xs <- getRandomCnstr cs
                       if null xs
                         then return []
                         else return
                           (extendNode' (s,cs\\xs) (head xs))

rsolveNs' :: [Node] -> IO [Node]
rsolveNs' ns = rsearch rsuccNode' solved (return ns)

genRandomSudoku' :: IO Node
genRandomSudoku' = do [r] <- rsolveNs' [emptyN']
                      return r

randomS' = genRandomSudoku' >>= showNode

uniqueSol' :: Node -> Bool
uniqueSol' node = singleton (solveNs' [node]) where
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseN' :: Node -> (Row,Column) -> Node
eraseN' n (r,c) = (s, constraints' s)
  where s = eraseS (fst n) (r,c)

minimalize' :: Node -> [(Row,Column)] -> Node
minimalize' n [] = n
minimalize' n ((r,c):rcs) | uniqueSol' n' = minimalize' n' rcs
                          | otherwise     = minimalize' n  rcs
  where n' = eraseN' n (r,c)

genProblem' :: Node -> IO Node
genProblem' n = do ys <- randomize xs
                   return (minimalize' n ys)
   where xs = filledPositions (fst n)

solveNs' :: [Node] -> [Node]
solveNs' = search succNode' solved

succNode' :: Node -> [Node]
succNode' (s,[]) = []
succNode' (s,p:ps) = extendNode' (s,ps) p

solveAndShow' :: Grid -> IO[()]
solveAndShow' gr = solveShowNs' (initNode' gr)

solveShowNs' :: [Node] -> IO[()]
solveShowNs' = sequence . fmap showNode . solveNs'

initNode' :: Grid -> [Node]
initNode' gr = let s = grid2sud gr in
               if (not . consistent') s then []
               else [(s, constraints' s)]

extendNode' :: Node -> Constraint -> [Node]
extendNode' (s,constraints') (r,c,vs) =
   [(extend s ((r,c),v),
     sortBy length3rd $
         prune' (r,c,v) constraints') | v <- vs ]

prune' :: (Row,Column,Value)
      -> [Constraint] -> [Constraint]
prune' _ [] = []
prune' (r,c,v) ((x,y,zs):rest)
  | sameConstrnt (r,c) (x,y) =
        (x,y,zs\\[v]) : prune' (r,c,v) rest
  | otherwise = (x,y,zs) : prune' (r,c,v) rest

-- | Use sameConstrnt instead of sameblock
-- First filter the constraints to find the right constraint for the position.
-- Next check if constraint matches
sameConstrnt :: (Row,Column) -> (Row,Column) -> Bool
sameConstrnt (r,c) (x,y) = any (elem (x,y)) $ filter (elem (r,c)) allConstrnt
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
-- Exercise 5 :: Time spent: 1+ hour
-- Same approach as Exercise 1
-- =============================================================================
exercise5 = do [r] <- rsolveNsNRC [emptyNNRC]
               showNode r
               s  <- genProblemNRC r
               showNode s

emptyNNRC :: Node
emptyNNRC = (\ _ -> 0,constraintsNRC (\ _ -> 0))

rsolveNsNRC :: [Node] -> IO [Node]
rsolveNsNRC ns = rsearch rsuccNodeNRC solved (return ns)

rsuccNodeNRC :: Node -> IO [Node]
rsuccNodeNRC (s,cs) = do xs <- getRandomCnstr cs
                         if null xs
                           then return []
                           else return
                             (extendNodeNRC (s,cs\\xs) (head xs))


uniqueSolNRC :: Node -> Bool
uniqueSolNRC node = singleton (solveNsNRC [node]) where
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseNNRC :: Node -> (Row,Column) -> Node
eraseNNRC n (r,c) = (s, constraintsNRC s)
  where s = eraseS (fst n) (r,c)

minimalizeNRC :: Node -> [(Row,Column)] -> Node
minimalizeNRC n [] = n
minimalizeNRC n ((r,c):rcs) | uniqueSolNRC n' = minimalizeNRC n' rcs
                            | otherwise    = minimalizeNRC n  rcs
  where n' = eraseNNRC n (r,c)

genProblemNRC :: Node -> IO Node
genProblemNRC n = do ys <- randomize xs
                     return (minimalizeNRC n ys)
   where xs = filledPositions (fst n)

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
