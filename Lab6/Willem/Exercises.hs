module Lab6 where

import Lecture6
import System.Clock

-- Define Main --
main = do
    putStrLn "===================="
    putStrLn "Assignment 6 / Lab 6"
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
    putStrLn "> Exercise 6 (1)"
    exercise6
    putStrLn "> Exercise 6 (2)"
    exercise62
    putStrLn "> Exercise 7 (BONUS)"
    exercise7

-- =============================================================================
-- Exercise 1 :: Time spent: 1 hour
-- =============================================================================

exercise1 = do
    print(exM 123 33 5)

-- =============================================================================
-- Exercise 2 :: Time spent: +-
-- =============================================================================
exercise2 = do
  expMTime <- testTime $ return $ expM 123 33 5
  exMTime  <- testTime $ return $ exM  123 33 5
  putStrLn "Time taken by original:"
  print expMTime
  putStrLn "Time taken by refactored:"
  print exMTime
  print()

-- | testTime function developed in Lab5 by me
testTime :: IO a -> IO (TimeSpec)
testTime f = do
  start <- getTime Monotonic
  f
  end <- getTime Monotonic
  return (diffTimeSpec start end)

-- =============================================================================
-- Exercise 3 :: Time spent: 15 min
-- =============================================================================
-- | Simply filter out the primes
composites :: [Integer]
composites = filter (not.prime) [3..]

-- | Show composite numbers up to 150, checked according to Wikipedia
exercise3 = putStrLn $ show (take 114 Lab6.composites)

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
-- Exercise 6 (1) :: Time spent: +-
-- =============================================================================
exercise6 = do
  print()

-- =============================================================================
-- Exercise 6 (2) :: Time spent: +-
-- =============================================================================
exercise62 = do
  print()

-- =============================================================================
-- Exercise 7 :: Time spent: +-
-- =============================================================================
exercise7 = do
  print()
