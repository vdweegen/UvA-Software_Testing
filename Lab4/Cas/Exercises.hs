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
  x <- set
  y <- set
  print $ x
  print $ y
  print $ intersectionSet x y
  print $ differenceSet x y

-- little helper
set2list :: Ord a => Set a -> [a]
set2list (Set []) = []
set2list (Set (x:xs)) = x : set2list (Set xs)

intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set []) set2 = Set []
intersectionSet set1 (Set []) = Set []
intersectionSet set1 set2 = list2set ((set2list set1) `intersect` (set2list set2))

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set []) set2 = set2
differenceSet set1 (Set []) = Set []
differenceSet set1 set2 = list2set ((set2list set1) \\ (set2list set2))


-- =============================================================================
-- Exercise 4 :: Time spent +-
-- =============================================================================
exercise4 = do
  print()

-- =============================================================================
-- Exercise 5 :: Time spent +-
-- =============================================================================
exercise5 = do
  let ex5list = [(1,2),(2,3),(3,4)] -- should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
  print $ symClos ex5list

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
 -- not sure if we should add 'sort.' before foldr, since the assignment states 'sorted'
symClos = foldr (\(a,b) x -> (a,b):(b,a):x) []

-- =============================================================================
-- Exercise 6 :: Time spent +-
-- =============================================================================
exercise6 = do
  let ex6list = [(1,2),(2,3),(3,4)]  -- should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
  print $ ex6list
  print $ ex6list @@ ex6list
  print $ trClos ex6list

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos r = if r == transitive then transitive else trClos transitive
  where transitive = nub $ (r ++ (r@@r))


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
