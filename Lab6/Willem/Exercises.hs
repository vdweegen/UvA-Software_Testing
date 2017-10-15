module Lab6 where

import Lecture6
import System.Clock
import Test.QuickCheck

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
    quickCheck prop_exm

prop_exm :: (Positive Integer, Positive Integer, Positive Integer) -> Bool
prop_exm (Positive a, Positive b, Positive c) = exM a b c == expM a b c

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
-- Exercise 4 :: Time spent: 30 mins
-- =============================================================================
exercise4 = do
  k1 <- foolsFermat 1 Lab6.composites
  k2 <- foolsFermat 2 Lab6.composites
  k3 <- foolsFermat 3 Lab6.composites
  k4 <- foolsFermat 4 Lab6.composites
  print(k1)
  print(k2)
  print(k3)
  print(k4)

foolsFermat :: Int -> [Integer] -> IO Integer
foolsFermat k (c:cs) = do
                         check <- primeTestsF k c
                         if check then return c
                         else foolsFermat k cs

-- =============================================================================
-- Exercise 5 :: Time spent: +-
-- =============================================================================
exercise5 = do
  print()


carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1)
              | k <- [2..],
              prime (6*k+1),
              prime (12*k+1),
              prime (18*k+1) ]

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
