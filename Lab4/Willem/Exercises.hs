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

-- | Example 4.52 still fuzzy about the type inference

-- =============================================================================
-- Exercise 2 :: Time spent +- 2hours
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
-- Exercise 3 :: Time spent +- 2hours
-- =============================================================================
exercise3 = do
  quickCheck prop_union_subset
  quickCheck prop_union_associative
  quickCheck prop_union_diff
  quickCheck prop_intersect_subset
  quickCheck prop_intersect_associative
  quickCheck prop_difference_subset
  quickCheck prop_difference_notassociative

-- | Use unionSet from SetOrd.hs
-- Use intersect and \\ from Data.List
intersectionSet, differenceSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set []) _ = emptySet
intersectionSet _ (Set []) = emptySet
intersectionSet (Set xs) (Set ys) = Set (xs `intersect` ys)

differenceSet set1 (Set []) = set1
differenceSet (Set []) _ = emptySet
differenceSet (Set xs) (Set ys) = Set (xs  \\ ys)

-- | Union props
prop_union_subset :: Set Int -> Set Int -> Bool
prop_union_subset a b = a `subSet` s && b `subSet` s where s = a `unionSet` b

prop_union_associative :: Set Int -> Set Int -> Bool
prop_union_associative a b = a `unionSet` b == b `unionSet`a

prop_union_diff :: Set Int -> Set Int -> Bool
prop_union_diff a b = (s `differenceSet` b) `subSet` a && (s `differenceSet` a) `subSet` b where s = a `unionSet` b

-- | Intersect props
prop_intersect_subset :: Set Int -> Set Int -> Bool
prop_intersect_subset a b = s `subSet` a && s `subSet` b where s = a `intersectionSet` b

prop_intersect_associative :: Set Int -> Set Int -> Bool
prop_intersect_associative a b = a `intersectionSet` b == b `intersectionSet` a

-- | Difference props
prop_difference_subset :: Set Int -> Set Int -> Bool
prop_difference_subset a b = s `subSet` a where s = a `differenceSet` b

prop_difference_notassociative :: Set Int -> Set Int -> Bool
prop_difference_notassociative a b = isEmpty ((a `differenceSet` b) `intersectionSet` (b `differenceSet` a))

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

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos = sort.nub.foldr (\(x,y) z -> (x,y):(y,x):z) []

-- =============================================================================
-- Exercise 6 :: Time spent +-
-- =============================================================================
exercise6 = do
  print()

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos xs | xs == transitive = sort xs
          | otherwise = trClos transitive
          where transitive = nub $ xs ++ (xs @@ xs)

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
