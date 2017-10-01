module Lab4 where

import Data.List
import System.Random
import Lab2.Lecture2
import Lab4.SetOrd
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- Define Main --
main = do
    putStrLn "===================="
    putStrLn "Assignment 4 / Lab 4"
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
    putStrLn "> Exercise 6"
    exercise6
    putStrLn "> Exercise 7"
    exercise7
    putStrLn "> Exercise 8"
    exercise8
    putStrLn "> Exercise 9"
    exercise9
    putStrLn "> Exercise 10"
    exercise10

-- =============================================================================
-- Exercise 1 :: Time spent: +- 3 hours
-- =============================================================================

exercise1 = print "Read chapter 4"

-- | Bauke van den Berg
-- Time spent: 4 hours on reading / constructing the questions
-- Things unclear after reading the chapter:
-- Example 4.6 / Exercise 4.7 => Meaning of symbols, and examples
-- Re-read of paragraph 4.4 required
-- Generalization of Union and Intersection
-- How to construct the proof / show that certain items are valid?
-- TODO: Handle the exercises 4.38, proving the Theorems

-- | Willem Jan Glerum
-- Example 4.52 still fuzzy about the type inference on how that works

-- =============================================================================
-- Exercise 2 :: Time spent +- 120 minutes
--                          + 20 minutes discussion
-- =============================================================================
exercise2 = do
  set <- getIntS 10 10
  putStrLn "Own generator:"
  print set
  putStrLn "Quickcheck:"
  sample(arbitrary :: Gen (Set Int))

-- | Re-use getIntL from Lecture2.hs and use the list2set from SetOrd.hs
-- Stop using the list2set and use nub and sort from Haskell
getIntS :: Int -> Int -> IO (Set Int)
getIntS k n = do
  l <- getIntL k n
  return(listToSet l)


-- | Helper function to transform a list to a set
listToSet :: (Eq a, Ord a) => [a] -> Set a
listToSet l = Set (sort $ nub l)

-- | Using Arbitrary from Quickcheck, based on Arbitrary instance from list
-- Using fmap to get a set
-- Mostly finding out that I needed to add Ord a to the instance
-- Bonus: this one also works for other types such as String, Char, Bool, Float, etc.
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = sized $ \n -> do
    k <- choose (0,n)
    listToSet `fmap` sequence [ arbitrary | _ <- [1..k] ]


-- =============================================================================
-- Exercise 3 :: Time spent +- 180 minutes
--                          + 60 minutes discussion
--                          + 60 minutes refactoring
-- =============================================================================
exercise3 = do
  putStrLn "Own generator:"
  testMan 1 100 randomSet unionSet isUnion
  testMan 1 100 randomSet intersectionSet isIntersection
  testMan 1 100 randomSet differenceSet isDifference
  quickCheckResult (prop_CheckSet prop_union_subset)
  quickCheckResult (prop_CheckSet prop_union_associative)
  quickCheckResult (prop_CheckSet prop_union_diff)
  quickCheckResult (prop_CheckSet prop_intersect_subset)
  quickCheckResult (prop_CheckSet prop_intersect_associative)
  quickCheckResult (prop_CheckSet prop_difference_subset)
  quickCheckResult (prop_CheckSet prop_difference_notassociative)
  putStrLn "quickCheck:"
  quickCheck prop_union_subset
  quickCheck prop_union_associative
  quickCheck prop_union_diff
  quickCheck prop_intersect_subset
  quickCheck prop_intersect_associative
  quickCheck prop_difference_subset
  quickCheck prop_difference_notassociative

-- ===============
-- Own Test
-- ===============
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

randomSet :: IO (Set Int)
randomSet = do
  p <- getStdGen                    -- used to randomly pick an item
  x <- getRandomInt 100             -- get a random int
  return $ list2set $ take x $ nub (randomRs (0, 500) p)

randomSetFixed :: Int -> IO (Set Int)
randomSetFixed n = do
  p <- getStdGen                    -- used to randomly pick an item
  x <- getRandomInt 100             -- get a random int
  return $ list2set $ take n $ take x $ nub (randomRs (0, 500) p)

-- Own test (adapted from Lab2)
testMan :: Integer -> Integer -> IO (Set Int) -> (Set Int -> Set Int -> Set Int)
  -> (Set Int -> Set Int -> Set Int -> Bool) -> IO ()
testMan k n i f r =
  if k == n then
    putStrLn ("+++ OK, passed " ++ show n ++ " tests")
  else do
    s1 <- i
    s2 <- i
    if r (f s1 s2) s1 s2 then
      testMan (k+1) n i f r
    else error ("[" ++ show s1 ++ "," ++ show s2 ++ "] failed after " ++ show k ++ " attempts")

prop_CheckSet :: (Set Int -> Set Int -> Bool) -> Positive Int -> Property
prop_CheckSet p n = monadicIO $ do
  result <- run (checkSet n p)
  assert result

checkSet :: Positive Int -> (Set Int -> Set Int -> Bool) -> IO Bool
checkSet (Positive n) p = do
  s1 <- randomSetFixed n
  s2 <- randomSetFixed n
  return $ p s1 s2

-- ===============
-- QuickTest Test
-- ===============
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
-- Exercise 4 :: Time spent +- 3 hours
-- =============================================================================
exercise4 = print "Read Chapter 5"

-- | Bauke van den Berg
-- Time spent: 3 hours on reading / trying out some haskell programs
-- Questions / Thins requiring additional lookup
-- Definitions with examples for relation types
-- Same as for chapter four, how to show / construct proof for the theorems / statements
-- Additional examples / study into correct notation of sets composed with relations
-- Study the notation / composition of 'derived' sets, such as inverse / closures

-- =============================================================================
-- Exercise 5 :: Time spent +- 10 minutes + 10 minutes discussion
-- Simply a recursive concatenation of the lists
-- Used sample exercise along with one with two equal terms to show no duplicates in a set
-- =============================================================================

type Rel a = [(a,a)]

exercise5 = do
  putStr "Example is correct: "
  print $ [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)] == symClos [(1,2),(2,3),(3,4)]
  putStr "No duplicates for equal terms: "
  print $ [(1,1)] == symClos [(1,1)]

symClos :: Ord a => Rel a -> Rel a
symClos = sort.nub.foldr (\(x,y) z -> (x,y):(y,x):z) []

-- =============================================================================
-- Exercise 6 :: Time spent +- 30 minutes
--                          + 15 minutes discussion
-- Same loop recursion utilizing the infixr operation
-- However, this could be solved using the fix / fp' from the workshop!
-- =============================================================================
inputRelation = [(1,2),(2,3),(3,4)]
expectedClosure = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

exercise6 = do
  putStr "Expecting transitive closure to be correct: "
  print $ expectedClosure == trClos inputRelation

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

trClos :: Ord a => Rel a -> Rel a
trClos xs | xs == result = sort xs
          | otherwise = trClos result
          where result = sort $ nub $ xs ++ (xs @@ xs)

-- =============================================================================
-- Exercise 7 :: Time spent +- 120 minutes
--                          + 30 minutes discussion
-- Adding a simple (hardly randomized) property to test
-- =============================================================================
exercise7 = do
  quickCheck prop_trClos_no_duplicates
  quickCheck prop_trClos_original
  quickCheck prop_trClos_ordered
  quickCheck prop_symClos_no_duplicates
  quickCheck prop_symClos_original
  quickCheck prop_symClos_ordered
  quickCheck prop_symClos_swapped

-- | We directly use QuickCheck to test our functions as it is quite easy to generate the relations.
-- We define some properties which we can test.

-- | First try to implement my own Arbitrary, however found out that these are
-- already provided because we have instances for lists and tuples of arbitrary.
-- Thus we can simply use them, since we are using the type alias Rel a
prop_trClos_no_duplicates :: Rel Int -> Bool
prop_trClos_no_duplicates a = nub b == b where b = trClos a

prop_trClos_original :: Rel Int -> Bool
prop_trClos_original a = all (`elem` b) a where b = trClos a

prop_trClos_ordered :: Rel Int -> Bool
prop_trClos_ordered a = sort b == b where b = trClos a

prop_symClos_no_duplicates :: Rel Int -> Bool
prop_symClos_no_duplicates a = nub b == b where b = symClos a

prop_symClos_original :: Rel Int -> Bool
prop_symClos_original a = all (`elem` b) a where b = symClos a

prop_symClos_ordered :: Rel Int -> Bool
prop_symClos_ordered a = sort b == b where b = symClos a

prop_symClos_swapped :: Rel Int -> Bool
prop_symClos_swapped a = all (\(x,y) -> (y,x) `elem` b) a where b = symClos a

-- =============================================================================
-- Exercise 8 :: Time spent +- 60 minutes
-- Didn't even try to manually find counter examples
-- Relied on the quickCheck generator to generate an example every time
-- Keyed in the random generators + quickCheck property
-- Took some time to fix the syntactic sugar of the nested do loops
-- Note: Intentionally returns TRUE on both occasions, simply to not 'break' the loop on the first error
-- Looking at the printed counter examples, one can immediately see why the transitive closure of the
-- symmetric closure returns more items than the symmetric closure of the transitive closure
-- The symmetric closure of a (a,b) results in both (a,b) and (b,a)
-- When mapping the transitivity on this result, relations from (a,a) and (b,b) are found.
-- The transitive closure of (a,b) is still (a,b). Making that symmetric
-- Will result in only (a,b) and (b,a). This specific example added below
-- =============================================================================
exercise8 = do
  quickCheckWith stdArgs {maxSize = 10} prop_checkCompare
  simpleCounterExample

simpleCounterExample :: IO ()
simpleCounterExample = do
  let relation = [(0,1)]
  putStrLn $ "Simple counter example: " ++ show relation
  let stc = symTrClos relation
  let rsc = trSymClos relation
  putStrLn $ show stc ++ " /= " ++ show rsc

prop_checkCompare n = monadicIO $ do
  result <- run (checkComparison n)
  assert result

checkComparison :: Int -> IO Bool
checkComparison n = do
  rels <- randomRelations n
  let stClos = symTrClos rels
  let tsClos = trSymClos rels
  if stClos /= tsClos
  then do
    putStr "Error when checking: "
    print rels
    putStr "symmetric transitive: "
    print stClos
    putStr "transitive symmetric: "
    print tsClos
    return True
  else return True

-- | Generate some random values composing a set of maximum n elements
randomRelations :: Int -> IO (Rel Int)
randomRelations n = do
    rels <- sequence [ randomRelation n | a <- [1..n]]
    return $ nub rels

randomInt :: Int -> IO Int
randomInt n = randomRIO (0, n)

-- | Some random mapping from (a,b)
randomRelation :: Int -> IO (Int, Int)
randomRelation n = do
            a <- randomInt n
            b <- randomInt n
            return (a,b)

-- | First transitive closure, then symmetric
symTrClos :: Ord a => Rel a -> Rel a
symTrClos = symClos . trClos

-- | First symmetric, then transitive closure
trSymClos :: Ord a => Rel a -> Rel a
trSymClos = trClos . symClos

-- =============================================================================
-- Exercise 9 :: Time spent +- 5 minutes
-- =============================================================================

-- We only had a quick look and skipped it
exercise9 = do
  print()

-- =============================================================================
-- Exercise 10 :: Time spent +- 30 minutes
-- =============================================================================

-- | Random difficult problem. However we were not able to solve the solution for 10^15

exercise10 = print $ "Hello"

possibleFinishes n | n == 0 = 0
                   | otherwise = totalFinishes n + totalFinishes (n-1)

totalFinishes n = amountOfSingleFinishes n + amountOfDoubleFinishes n + amountOfTripleFinishes n


amountOfTripleFinishes n | n == 0 = 0
                         | otherwise = computeTripleFinishes n + amountOfTripleFinishes (n-1)

computeTripleFinishes :: Integer -> Integer
computeTripleFinishes n = sum $ [ finisheable rest | firstDart <- allPossibleValues, secondDart <- allPossibleValues, let rest = (n - firstDart - secondDart)]

amountOfDoubleFinishes :: Integer -> Integer
amountOfDoubleFinishes n | n == 0 = 0
                         | otherwise = computeFinishes n + amountOfDoubleFinishes (n-1)

computeFinishes :: Integer -> Integer
computeFinishes n = sum $ [ finisheable rest | firstDart <- allPossibleValues, let rest = (n - firstDart) ]


amountOfSingleFinishes :: Integer -> Integer
amountOfSingleFinishes n | n == 0 = 0
                         | otherwise = finisheable n + amountOfSingleFinishes (n-1)

finisheable :: Integer -> Integer
finisheable n | n `elem` doubleValues = 1
              | otherwise = 0

doubleValues :: [Integer]
doubleValues = [ n + n | n <- [2,4..40]] ++ [50]

allPossibleValues :: [Integer]
allPossibleValues = [1..20] ++ [2,4..40] ++ [3,6..60] ++ [25] ++ [50]
