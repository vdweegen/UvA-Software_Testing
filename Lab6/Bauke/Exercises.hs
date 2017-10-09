module Lab6 where

import Bauke.Lecture6

import System.IO.Unsafe (unsafeInterleaveIO)
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
  print()


exM :: Integer -> Integer -> Integer -> Integer
exM a b c = (a^b) `mod` c
-- =============================================================================
-- Exercise 2 :: Time spent: +-
-- =============================================================================
exercise2 = do
  print()

-- =============================================================================
-- Exercise 3 :: Time spent: +- 5 minutes
-- Simply write a list comprehension containing all non primes
-- Checked the implementation against the wikipedia link for the known composites up to 150.
-- These are exactly equal
-- =============================================================================
exercise3 = do
  putStr "Checking composites against known values up to 150: "
  print $ verifyComposites

verifyComposites :: Bool
verifyComposites = (takeWhile (<=150) composites) == firstComposites

firstComposites :: [Integer]
firstComposites = [4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20, 21, 22, 24, 25, 26, 27, 28, 30, 32, 33, 34, 35, 36, 38, 39, 40, 42, 44, 45, 46, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 60, 62, 63, 64, 65, 66, 68, 69, 70, 72, 74, 75, 76, 77, 78, 80, 81, 82, 84, 85, 86, 87, 88, 90, 91, 92, 93, 94, 95, 96, 98, 99, 100, 102, 104, 105, 106, 108, 110, 111, 112, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 128, 129, 130, 132, 133, 134, 135, 136, 138, 140, 141, 142, 143, 144, 145, 146, 147, 148, 150]
-- =============================================================================
-- Exercise 4 :: Time spent: +- 30 minutes
-- Took some time to generate a list of items from an infinite list.
-- Found an example only showing this using a filter.
-- When the k is increased, the value of the prime increases.
-- =============================================================================
exercise4 = do
  putStr "Smallest non-prime composite number k=[1..3]: "
  primes <- falsePrimes 3
  let falsePrime = head primes
  print falsePrime
  putStr "Smalles non-prime composite number k=[1..5]: "
  primes' <- falsePrimes 5
  let falsePrime' = head primes'
  print falsePrime'

falsePrimes :: Int -> IO [Integer]
falsePrimes n = filterMIO (primeTestsF n) composites

filterMIO :: (a -> IO Bool) -> [a] -> IO [a]
filterMIO p = go
  where
    go []     = return []
    go (x:xs) = do
      xs' <- unsafeInterleaveIO (go xs)
      b   <- p x
      return $ if b then (x:xs') else xs'

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
