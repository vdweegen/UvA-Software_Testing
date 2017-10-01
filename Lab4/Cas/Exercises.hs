module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Lab4.SetOrd
import Lab4.Lecture4

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
  putStrLn "Read Chapter 4"
-- how can one test operations on list comprehensions that simulate infinity?
--

-- =============================================================================
-- Exercise 2 :: Time spent +-
-- =============================================================================
exercise2 = do
  x <- set
  print x

-- 'Borrowed from Lecture2'
getRandomInt :: Int -> IO Int
getRandomInt n = randomRIO (0,n)

set :: IO (Set Int)
set = do
  p <- getStdGen                    -- used to randomly pick an item
  x <- getRandomInt 100             -- get a random int
  return $ list2set $ take x $ nub (randomRs (0, 500) p)

fixedLengthSet :: Int -> IO (Set Int)
fixedLengthSet n = do
  p <- getStdGen                    -- used to randomly pick an item
  x <- getRandomInt 100             -- get a random int
  return $ list2set $ take n $ take x $ nub (randomRs (0, 500) p)

-- =============================================================================
-- Exercise 3 :: Time spent +-
-- =============================================================================
exercise3 = do
  putStrLn "Manual:"
  test 1 100 set intersectionSet isIntersection
  test 1 100 set differenceSet isDifference
  test 1 100 set unionSet isUnion
  putStrLn "quickCheck:"
  quickCheckResult prop_checkIntersection
  -- verboseCheckResult prop_checkIntersection
  quickCheckResult prop_checkDifference
  -- verboseCheckResult prop_checkDifferece
  quickCheckResult prop_checkUnion
  -- verboseCheckResult prop_checkDifferece

differenceBug = do
  putStr "Bug is fixed: "
  print $ (Set []) /= differenceSet (Set [0]) (Set[0,1,2])

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

-- test intersection (all items should be in both sets)
isIntersection :: Ord a => Set a -> Set a -> Set a -> Bool
isIntersection (Set []) _ _ = True
isIntersection (Set (x:xs)) s2 s3
  | inSet x s2 && inSet x s3 = isIntersection (Set xs) s2 s3
  | otherwise = False

-- test difference (all items in set a should not be in set b)
isDifference :: Ord a => Set a -> Set a -> Set a -> Bool
isDifference _ (Set []) _ = True
isDifference (Set []) _ _ = True
isDifference s (Set (x:xs)) s3
  | inSet x s = isDifference s (Set xs) s3
  | not(inSet x s) && inSet x s3 = isDifference s (Set xs) s3
  | otherwise = False

-- test union (all items should be in either a or b)
isUnion :: Ord a => Set a -> Set a -> Set a -> Bool
isUnion (Set []) _ _ = True
isUnion (Set (x:xs)) s2 s3
  | inSet x s2 = isUnion (Set xs) s2 s3
  | inSet x s3 = isUnion (Set xs) s2 s3
  | otherwise = False

-- Own test (adapted from Lab2)
test :: Integer -> Integer -> IO (Set Int) -> (Set Int -> Set Int -> Set Int)
  -> (Set Int -> Set Int -> Set Int -> Bool) -> IO ()
test k n i f r =
  if k == n then
    print (show n ++ " tests passed")
  else do
    s1 <- i
    s2 <- i
    if r (f s1 s2) s1 s2 then
      do test (k+1) n i f r
    else error ("[" ++ show s1 ++ "," ++ show s2 ++ "] failed after " ++ (show k) ++ " attempts")

-- QuickCheck Tests
prop_checkIntersection n = monadicIO $ do
  result <- run (checkIntersection n)
  assert (result)

prop_checkDifference n = monadicIO $ do
  result <- run (checkDifference n)
  assert (result)

prop_checkUnion n = monadicIO $ do
  result <- run (checkUnion n)
  assert (result)

checkIntersection :: Positive Int -> IO Bool
checkIntersection (Positive n) = do
  s1 <- fixedLengthSet n
  s2 <- fixedLengthSet n
  if isIntersection (intersectionSet s1 s2) s1 s2 then
    do return True
  else return False

checkDifference :: Positive Int -> IO Bool
checkDifference (Positive n) = do
  s1 <- fixedLengthSet n
  s2 <- fixedLengthSet n
  if isDifference (differenceSet s1 s2) s1 s2 then
    do return True
  else return False

checkUnion :: Positive Int -> IO Bool
checkUnion (Positive n) = do
  s1 <- fixedLengthSet n
  s2 <- fixedLengthSet n
  if isUnion (unionSet s1 s2) s1 s2 then
    do return True
  else return False

-- =============================================================================
-- Exercise 4 :: Time spent +-
-- =============================================================================
exercise4 = do
  putStrLn "Read Chapter 5"

-- =============================================================================
-- Exercise 5 :: Time spent +-
-- =============================================================================
exercise5 = do
  let list = [(1,2),(2,3),(3,4)] -- should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
  print $ symClos list

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos = sort . nub . foldr (\(a,b) x -> (a,b):(b,a):x) []

-- =============================================================================
-- Exercise 6 :: Time spent +-
-- =============================================================================
exercise6 = do
  let list = [(1,2),(2,3),(3,4)]  -- should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
  -- print $ list
  -- print $ list @@ list
  print $ trClos list

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos r = if r == transitive then transitive else trClos transitive
  where transitive = sort $ nub $ (r ++ (r@@r))

-- =============================================================================
-- Exercise 7 :: Time spent +-
-- =============================================================================
exercise7 = do
  a <- fixedLengthSet 10
  b <- fixedLengthSet 10
  -- print(a)
  -- print(b)
  print $ getRelations a b

-- The below generates a random relation, based on two sets
getRelations :: (Ord a) => Set a -> Set a -> Rel a
getRelations (Set []) _ = []
getRelations _ (Set []) = []
getRelations (Set (x:xs)) (Set (y:ys)) = (x,y) : getRelations (list2set xs) (list2set ys)

-- =============================================================================
-- Exercise 8 :: Time spent +-
-- =============================================================================

-- The symmetric closure of a transitive closure of a set cannot be the same as
-- a transitive closure of a symmetric closure due to the fact that (even for
-- a single relation) a symmetric closure required that (a <=> b) where a
-- transitive close only requires (a => b) meaning that a set of relations the
-- follow from a symmetric close is always larger than a transitive closure.

-- Three simple examples that disprove the statement are given below

exercise8 = do
  let list = [(1,2)]
  let list2 = [(1,2),(2,3)]
  let list3 = [(1,2),(2,3),(3,4)]
  print $ (symClos $ trClos list) == (trClos $ symClos list)
  print $ (symClos $ trClos list2) == (trClos $ symClos list2)
  print $ (symClos $ trClos list3) == (trClos $ symClos list3)
  quickCheckResult prop_checkCompare
  -- verboseCheckResult prop_checkCompare

prop_checkCompare n = monadicIO $ do
  result <- run (checkComparison n)
  assert (result)

checkComparison :: Positive Int -> IO Bool
checkComparison (Positive n) = do
  a <- fixedLengthSet n
  b <- fixedLengthSet n
  let list = getRelations a b
  if (symClos $ trClos list) == (trClos $ symClos list) && (list /= list)
  then do
    -- print $ list
    -- print $ symClos $ trClos list
    -- print $ trClos $ symClos list
    return False
  else return True


-- =============================================================================
-- Exercise 9 :: Time spent +-
-- =============================================================================
exercise9 = do
  print()

-- basic Show definition
instance Show Statement where
  show s = ""

-- =============================================================================
-- Exercise 10 :: Time spent +-
-- =============================================================================
exercise10 = do
  print()
