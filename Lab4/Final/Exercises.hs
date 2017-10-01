module Lab4 where

import Data.List
import System.Random
import Lab2.Lecture2
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

exercise1 = print $ "Read chapter 4"

-- | Bauke van den Berg
-- Time spent: 4 hours on reading / constructing the questions
-- Things unclear after reading the chapter:
-- Example 4.6 / Exercise 4.7 => Meaning of symbols, and examples
-- Re-read of paragraph 4.4 required
-- Generalization of Union and Intersection
-- How to construct the proof / show that certain items are valid?
-- TODO: Handle the exercises 4.38, proving the Theorems

-- =============================================================================
-- Exercise 2 :: Time spent +- 120 minutes + 20 minutes discussion
-- =============================================================================
exercise2 = do
  set <- getIntS 10 10
  putStrLn "Manual:"
  print set
  putStrLn "Quickcheck:"
  sample(arbitrary :: Gen (Set Int))

-- | Re-use getIntL from Lecture2.hs and use the list2set from SetOrd.hs
-- Time spent 10 mins
-- Stop using the list2set and use nub and sort from Haskell
getIntS :: Int -> Int -> IO (Set Int)
getIntS k n = do
  l <- getIntL k n
  return(listToSet l)


-- | Helper function to transform a list to a set
-- Time spent 15 mins to remove the list2set
listToSet :: (Eq a, Ord a) => [a] -> Set a
listToSet l = Set (sort $ nub l)

-- | Using Arbitrary from Quickcheck, based on Arbitrary instance from list
-- Using fmap to get a set
-- Time spent 1 hour, mostly finding out that I needed to add Ord a to the instance
-- Bonus: this one also works for other types such as String, Char, Bool, Float, etc.
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = sized $ \n -> do
    k <- choose (0,n)
    listToSet `fmap` sequence [ arbitrary | _ <- [1..k] ]


-- =============================================================================
-- Exercise 3 :: Time spent +-
-- =============================================================================
exercise3 = do
  print()
-- =============================================================================
-- Exercise 4 :: Time spent +-
-- =============================================================================
exercise4 = print $ "Read Chapter 5"

-- | Bauke van den Berg
-- Time spent: 3 hours on reading / trying out some haskell programs
-- Questions / Thins requiring additional lookup
-- Definitions with examples for relation types
-- Same as for chapter four, how to show / construct proof for the theorems / statements
-- Additional examples / study into correct notation of sets composed with relations
-- Study the notation / composition of 'derived' sets, such as inverse / closures

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
