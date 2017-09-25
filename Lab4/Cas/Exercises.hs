module Lab4 where

import Data.List
import System.Random

import Lab4.SetOrd

import Test.QuickCheck
-- Define Main --
main = do
    putStrLn $ "===================="
    putStrLn $ "Assignment 4 / Lab 4"
    putStrLn $ "===================="
    putStrLn $ "> Exercise 1"
    exercise1
    putStrLn $ "> Exercise 2"
    exercise2
    putStrLn $ "> Exercise 3"
    exercise3
    putStrLn $ "> Exercise 4"
    exercise4
    putStrLn $ "> Exercise 5"
    exercise5
    putStrLn $ "> Exercise 6"
    exercise6
    putStrLn $ "> Exercise 7"
    exercise7
    putStrLn $ "> Exercise 8"
    exercise8
    putStrLn $ "> Exercise 9"
    exercise9
    putStrLn $ "> Exercise 10"
    exercise10

-- =============================================================================
-- Exercise 1 :: Time spent: +-
-- =============================================================================

exercise1 = do
  print()

-- =============================================================================
-- Exercise 2 :: Time spent +-
-- =============================================================================
exercise2 = do
  x <- set
  print x

-- 'Borrowed from Lecture2'
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

set :: IO (Set Int)
set = do
  p <- getStdGen                    -- used to randomly pick an item
  x <- getRandomInt 100             -- get a random int
  return $ list2set $ take x (randomRs (-500, 500) p)

-- =============================================================================
-- Exercise 3 :: Time spent +-
-- =============================================================================
exercise3 = do
  print()
-- =============================================================================
-- Exercise 4 :: Time spent +-
-- =============================================================================
exercise4 = do
  print()

-- =============================================================================
-- Exercise 5 :: Time spent +-
-- =============================================================================
exercise5 = do
  print()

-- =============================================================================
-- Exercise 6 :: Time spent +-
-- =============================================================================
exercise6 = do
  print()

-- =============================================================================
-- Exercise 7 :: Time spent +-
-- =============================================================================
exercise7 = do
  print()

-- =============================================================================
-- Exercise 8 :: Time spent +-
-- =============================================================================
exercise8 = do
  print()

-- =============================================================================
-- Exercise 9 :: Time spent +-
-- =============================================================================
exercise9 = do
  print()

-- =============================================================================
-- Exercise 10 :: Time spent +-
-- =============================================================================
exercise10 = do
  print()
