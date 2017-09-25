module Lab4 where

import Data.List
import System.Random

import Lab4.SetOrd

import Test.QuickCheck
import Test.QuickCheck.Monadic

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

exercise1 = print ()


-- =============================================================================
-- Exercise 2 :: Time spent +-
-- =============================================================================
exercise2 = randomInt >>= (\n -> randomIntegers n)

randomSet :: Int -> IO (Set Int)
randomSet n = do
                xs <- randomIntegers n
                return $ Set xs

randomIntegers :: Int -> IO [Int]
randomIntegers n = sequence [ randomInt | a <- [1..n]]

randomInt :: IO Int
randomInt = randomRIO (0, 100)

-- =============================================================================
-- Exercise 3 :: Time spent +-
-- =============================================================================
exercise3 = do
  quickCheck prop_intersectionLength
  quickCheck prop_differenceLength

-- | Generates an intersection of two random sets
someIntersection :: IO (Set Int)
someIntersection = do
            xs <- randomSet 10
            ys <- randomSet 10
            return $ interSection xs ys

-- | Generates a difference of two random sets
someDifference :: IO (Set Int)
someDifference = do
            xs <- randomSet 10
            ys <- randomSet 10
            return $ difference xs ys

interSection, difference :: Ord a => Set a -> Set a -> Set a
interSection (Set xs) (Set ys) = (Set (intersect xs ys))
difference (Set xs) (Set ys) = (Set ((\\) xs ys))

prop_intersectionLength (Positive n) = monadicIO $ do
  result <- run $ validateIntersectionLength n
  assert (result)

validateIntersectionLength :: Int -> IO Bool
validateIntersectionLength n = do
  xs <- randomSet n
  ys <- randomSet n
  return $ (size xs) >= (size (interSection xs ys))

prop_differenceLength (Positive n) = monadicIO $ do
  result <- run $ validateDifferenceLength n
  assert (result)

validateDifferenceLength :: Int -> IO Bool
validateDifferenceLength n = do
  xs <- randomSet n
  ys <- randomSet n
  return $ (size xs) >= (size (difference xs ys))

size :: Ord a => Set a -> Int
size (Set xs) = length xs



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
