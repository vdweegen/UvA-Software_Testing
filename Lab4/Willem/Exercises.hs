module Lab4 where

import Data.List
import System.Random
import Lab2.Lecture2
import Lab4.SetOrd
import Control.Monad

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
  set <- getIntS 10 10
  putStrLn("Manual:")
  print(set)
  putStrLn("Quickcheck:")
  sample(arbitrary :: Gen (Set Int))

-- | Re-use getIntL from Lecture2.hs and use the list2set from SetOrd.hs
-- Time spent 10 mins
-- Stop using the list2set and use nub and sort from Haskell
getIntS :: Int -> Int -> IO (Set Int)
getIntS k n = do
  l <- getIntL k n
  return(Set (sort $ nub l))

-- | Using Arbitrary from Quickcheck, based on Arbitrary instance from list
-- Using fmap to get a set
-- Time spent 1 hour, mostly finding out that I needed to add Ord a to the instance
-- Bonus: this one also works for other types such as String, Char, Bool, Float, etc.
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = sized $ \n -> do
    k <- choose (0,n)
    list2set `fmap` (sequence [ arbitrary | _ <- [1..k] ])

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
