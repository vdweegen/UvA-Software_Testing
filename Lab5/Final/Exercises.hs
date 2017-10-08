module Lab5 where

import Lecture5
import Lecture5NRC
import Lecture5'
import Example
import System.Clock

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
-- See Lecture5NRC.hs for code and comments
-- =============================================================================
exercise1 = solveAndShowNRC example

-- | SOLUTION
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
-- Exercise 2 :: Time spent: +- 1.5 hours for refactoring, 1 hour to test
-- See Lecture5NRC'.hs for code and comments

-- The refactored version with the new constraints is more easy to extend, as we
-- only have to add new constraints, instead of adding new functions everywhere
-- to add a new constraint, like in Exercise 1.

-- The original version is more efficient at solving problems according to the
-- TimeSpec tests.
-- =============================================================================
exercise2 = do
              solveAndShow' example
              res <- testDiff
              print res

testDiff :: IO (Bool, Bool)
testDiff = do
            [r] <- rsolveNs [emptyN]
            showNode r
            refactored <- genProblem' r
            original <- genProblemNRC r
            testRefactored <- (testTime $ showNode refactored)
            testOriginal <- (testTime $ showNode original)
            solveRefactored <- (testTime $ solveAndShow' (sud2grid $ fst refactored))
            solveOriginal <- (testTime $ solveAndShowNRC (sud2grid $ fst original))
            return (testRefactored > testOriginal, solveRefactored > solveOriginal)

testTime :: IO a -> IO (TimeSpec)
testTime f =
  do start <- getTime Monotonic
     f
     end <- getTime Monotonic
     return (diffTimeSpec start end)

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
-- See Lecture5NRC.hs for code and comments
-- =============================================================================
exercise5 = genProblemAndShowNRC

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
