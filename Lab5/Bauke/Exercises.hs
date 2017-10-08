module Lab5 where

import Data.List
import System.Random

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
-- Exercise 1 :: Time spent: 2 hours on the validator, 2 hours on the solution.
-- Spent some time on copying the data from the
-- original lecture, trying to grasp what's going on. The order of functions are really
-- of no help, neither is the absence of proper naming / comments.
-- Used the
-- Merged partial solutions from team members with some 'own findings'.
-- Included the automatic grid checker to verify the solver against my own constaints
-- Restructured the original lecture's solution to 'use first, define later structure'
-- =============================================================================

puzzle1 :: Grid
puzzle1 = [[0,0,0,3,0,0,0,0,0],
           [0,0,0,7,0,0,3,0,0],
           [2,0,0,0,0,0,0,0,8],
           [0,0,6,0,0,5,0,0,0],
           [0,9,1,6,0,0,0,0,0],
           [3,0,0,0,7,1,2,0,0],
           [0,0,0,0,0,0,0,3,1],
           [0,8,0,0,4,0,0,0,0],
           [0,0,2,0,0,0,0,0,0]]

exercise1 = solveAndShow puzzle1



-- | Imported code for the solver

-- | Data types
type Node = (Sudoku,[Constraint])
type Constraint = (Row,Column,[Value])
type Sudoku = (Row,Column) -> Value
type Grid   = [[Value]]
type Value  = Int
type Row    = Int
type Column = Int

-- | Input data
positions, values :: [Int]
positions = [1..9]
values    = [1..9]

defaultBlocks, nrcBlocks :: [[Int]]
defaultBlocks = [[1..3],[4..6],[7..9]]
nrcBlocks = [[2..4], [6..8]]

-- | Solver
solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

-- | Convert Grid to list of constraints?
initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in
              if (not . consistent) s then []
              else [(s, constraints s)]

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c)
  where
  pos :: [[a]] -> (Row,Column) -> a
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) |
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ nrcSubgridInjective s (r,c) |
                   r <- [2,6], c <- [2,6] ]

nrcSubgridInjective :: Sudoku -> (Row,Column) -> Bool
nrcSubgridInjective s (r,c) = injective vs where
   vs = filter (/= 0) (nrcSubGrid s (r,c))

rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs where
   vs = filter (/= 0) (subGrid s (r,c))

subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) =
  [ s (r',c') | r' <- bl r, c' <- bl c ]

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

constraints :: Sudoku -> [Constraint]
constraints s = sortBy length3rd
    [(r,c, freeAtPos s (r,c)) |
                       (r,c) <- openPositions s ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) =
  (freeInRow s r)
   `intersect` (freeInColumn s c)
   `intersect` (freeInSubgrid s (r,c))
   `intersect` (freeInNrcSubgrid s (r,c))

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r =
  freeInSeq [ s (r,i) | i <- positions  ]

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

freeInNrcSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInNrcSubgrid s (r,c) = freeInSeq (nrcSubGrid s (r,c))

nrcSubGrid :: Sudoku -> (Row,Column) -> [Value]
nrcSubGrid s (r,c) =
  [ s (r',c') | r' <- blNrc r, c' <- blNrc c ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c =
  freeInSeq [ s (i,c) | i <- positions ]

freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq

-- | Return positions with no value
openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,
                            c <- positions,
                            s (r,c) == 0 ]

-- | Solve and show nodes ?
solveShowNs :: [Node] -> IO[()]
solveShowNs = sequence . fmap showNode . solveNs

showNode :: Node -> IO()
showNode = showSudoku . fst

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")
    putStr "Printed grid violates no NRC constraint: "
    print $ isValid [as,bs,cs,ds,es,fs,gs,hs,is]

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


showVal :: Value -> String
showVal 0 = " "
showVal d = show d

sud2grid :: Sudoku -> Grid
sud2grid s =
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ]

-- | Solve nodes ?
solveNs :: [Node] -> [Node]
solveNs = search succNode solved

-- | No idea...
succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p

-- | Split node into multiple nodes?
extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) =
   [(extend s ((r,c),v),
     sortBy length3rd $
         prune (r,c,v) constraints) | v <- vs ]

extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (y,z) x = if x == y then z else f x

-- | 'snoeien'
prune :: (Row,Column,Value)
      -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) =
        (x,y,zs\\[v]) : prune (r,c,v) rest
  | otherwise = (x,y,zs) : prune (r,c,v) rest


-- | Whether two cells are in the same 3x3 block
sameblock, sameDefaultBlock, sameNrcBlock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = sameDefaultBlock (r,c) (x,y) || sameNrcBlock (r,c) (x,y)
sameDefaultBlock (r,c) (x,y) = bl r == bl x && bl c == bl y
sameNrcBlock (r,c) (x,y) = blNrc r == blNrc x && blNrc c == blNrc y

bl, blNrc :: Int -> [Int]
bl x = concat $ filter (elem x) defaultBlocks
blNrc x = concat $ filter (elem x) nrcBlocks

-- | Sudoku is solved when the list of constraints is empty
solved  :: Node -> Bool
solved = null . snd

search :: (node -> [node])
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs)
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

-- | List of 3x3 squares to check for unique numbers
squares :: [(Int, Int)]
squares = [(1,1), (4,1), (7,1), (1,4), (4,4), (4,7), (1,7), (4,7), (7,7), (2,2), (6,2), (2,6), (6,6)]

isValid :: Grid -> Bool
isValid grid | not $ validGrid grid = False
             | 0 `elem` (concat grid) = False
             | dupesInLines grid = False
             | any (==False) $ map (validSquare grid) squares = False
             | otherwise = True

validGrid :: Grid -> Bool
validGrid grid | length grid /= 9 = False
               | (length $ concat grid) /= 81 = False
               | otherwise = True

dupesInLines :: Grid -> Bool
dupesInLines [] = False
dupesInLines (x:xs) = checkLine x && dupesInLines xs

checkLine :: [Int] -> Bool
checkLine [] = True
checkLine (0:_) = False
checkLine (x:xs) | x `elem` xs = False
                 | otherwise = checkLine xs

-- | Convenience call to flip list of rows to list of columns
flipGrid :: Grid -> Grid
flipGrid grid = compose $ [ value | start <- [1..9], step <- [0..8], let value = (concat grid) !! ((start-1) + (9*step)) ]

compose :: [Value] -> Grid
compose values = [ column | step <- [1..9], let column = drop ((step-1)*9) $ take (step*9) values]

-- | Convenience call to validate a 3x3 grid
validSquare :: Grid -> (Int,Int) -> Bool
validSquare grid (x,y) =  checkLine $ extractSquare grid (x,y)

extractSquare :: Grid -> (Int, Int) -> [Int]
extractSquare grid (x,y) = concat $ map (take 3) $ take 3 $ drop (y-1) $ map (drop (x-1)) grid

-- =============================================================================
-- Exercise 2 :: Time spent: +-
-- =============================================================================
exercise2 = do
  print()

-- =============================================================================
-- Exercise 3 :: Time spent: +-
-- The paper mentioned in the bonus mentions that computers have enough power to calculate
-- =============================================================================
exercise3 = do
  print()

-- =============================================================================
-- Exercise 4 :: Time spent: +-


-- | Sample code for the random generator
randomS = genRandomSudoku >>= showNode

genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0))

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                      if null xs
                        then return []
                        else return
                          (extendNode (s,cs\\xs) (head xs))

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs)
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

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




-- =============================================================================
-- Time spent: 45 minutes
-- Took a simple approach => use the random 'total solution generator'
-- This returns a solution. simply replace three random blocks with zeroes.
-- Note: The exercise does NOT specify the sudoku to be a minimal solution.
-- Hence, the generate can even generated sudoku's with no blocks...
-- =============================================================================
exercise4 = undefined


-- | Uses the provided generator to return a random grid
genRandomGrid :: IO Grid
genRandomGrid = do
  node <- genRandomSudoku
  return $ sud2grid $ fst node

-- | returns N random blocks to erase
randomBlocks :: Int -> IO [Int]
randomBlocks n = do
  gen <- newStdGen
  return $ take n $ nub $ (randomRs (1,9) gen :: [Int])

eraseBlocks :: [Int] -> Grid -> Grid
eraseBlocks [] grid = grid
eraseBlocks (x:xs) grid = eraseBlocks xs (eraseBlock x grid)

eraseBlock :: Int -> Grid -> Grid
eraseBlock 1 grid = undefined

-- | Convert rows to blocks
grid2blocks :: Grid -> Grid
grid2blocks grid = map (extractSquare grid) (take 9 squares)

--extractSquare :: Grid -> (Int, Int) -> [Int]
--extractSquare grid (x,y) = concat $ map (take 3) $ take 3 $ drop (y-1) $ map (drop (x-1)) grid

blocks2grid :: Grid -> Grid
blocks2grid blocks = [ extractRow blocks n | n <- [1..9] ]

-- | Extracts a row from block based grid
-- extractRow :: Grid -> Int -> [Int]
extractRow blocks row = concat $ [ take 3 $ drop (((3*(mod (row-1) 3))) + step*9) $ concat (drop (3*(div (row-1) 3)) blocks) | step <- [0..2]]

b :: Grid
b = grid2blocks puzzle1

row :: Int -> [Int]
row n = extractRow b n

rowValid :: Int -> Bool
rowValid n = (puzzle1 !! (n-1)) == (extractRow b n)

valid :: Bool
valid = puzzle1 == (blocks2grid $ grid2blocks puzzle1)

-- =============================================================================
-- Exercise 5 :: Time spent: +-
-- =============================================================================
exercise5 = do
  print()

-- =============================================================================
-- Exercise 6 :: Time spent: 1 hour on reading the paper
-- Based on this paper the difficulty of a sudoku is mainly based on two characteristics
-- 1) The techniques required to find the next step
-- 2) The amount of 'next' steps during a moment in the puzzle
-- =============================================================================
exercise6 = do
  print()

-- =============================================================================
-- Exercise 7 :: Time spent: +-
-- =============================================================================
exercise7 = do
  print()


