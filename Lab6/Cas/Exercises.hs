module Lab6 where

import Lecture6

import System.Random (randomRIO)
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
-- Exercise 1 :: Time spent: +-
-- =============================================================================
exercise1 = do
  print $ Lab6.exM 2 32 5

-- x is the base, y is the exponent, n is N, t is the term
exM :: Integer -> Integer -> Integer -> Integer
exM x 0 n = 1
exM x y n = t * Lab6.exM ((x * x) `mod` n) (y `div` 2) n `mod` n
  where t = if (not (y `mod` 2 == 0)) then x `mod` n else 1

-- =============================================================================
-- Exercise 2 :: Time spent: +-
-- =============================================================================
exercise2 = do
  testOriginal <- testTime $ mapM primeTestF (take 100000 primes)
  testRefactored <- testTime $ mapM primeTest (take 100000 primes)
  print $ "Testing 100000 primes with original code"
  print $ testOriginal
  print $ "Testing 100000 primes with improved code"
  print $ testRefactored

testTime :: IO a -> IO (TimeSpec)
testTime f = do
  start <- getTime Monotonic
  f
  end <- getTime Monotonic
  return (diffTimeSpec start end)

-- Modified version from Lecture6.hs
primeTest :: Integer -> IO Bool
primeTest n = do
   a <- randomRIO (2, n-1) :: IO Integer
   return (Lab6.exM a (n-1) n == 1)

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
