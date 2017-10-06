module Lab5 where

import Lab5.Lecture5

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
  putStrLn "Solution to the sudoku:"
  print $ "Fout"

squares :: [(Int, Int)]
squares = [(0,0), (3,0), (6,0), (0,3), (3,3), (3,6), (0,6), (3,6), (6,6)]

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
validSquare grid (x,y) =  checkLine $ concat $ map (take 3) $ take 3 $ drop y $ map (drop x) grid

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
