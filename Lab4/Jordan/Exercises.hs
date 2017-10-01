module Lab4 where

import Data.List
import System.Random
import Control.Monad
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
-- Exercise 1 :: Time spent: +- 10 and counting
-- =============================================================================

exercise1 = do
  print()

-- =============================================================================
-- Exercise 2 :: Time spent +- 3.30 hours
-- I had a hard time thinking about how to make a random generator. I spent the first 2 hours 
-- Trying to use the System.Random. Trying to create an instance that would work with the Set DataType
-- I came very close to solving it but gave up to work on the other exercises.
-- =============================================================================
exercise2 = do
  numberOfSets <- (getStdRandom (randomR (0, 5))) :: IO Int
  upperBounds <- (getStdRandom (randomR (0, 40))) :: IO Int
  seed <- (getStdRandom (randomR (0, 1000))) :: IO Int
  print $ take numberOfSets $ (map randomSet (randomInts upperBounds seed))
  sample $ (arbitrary :: Gen (Set Int))
-- From book  1
randomInts :: Int -> Int -> [Int]
randomInts bound seed =
  tail (randomRs (0,bound) (mkStdGen seed))

randomSet :: Int -> (Set Int)
randomSet n = Set listInts
      where 
      listInts = take n $ randomInts 100 10

instance (Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do
              list <- arbitrary
              return $ Set list

-- =============================================================================
-- Exercise 3 :: Time spent +- 20
-- =============================================================================
exercise3 = do
  print $ diff [1,2,3] [2,3,4]
  print $ diff' [1,2,3] [2,3,4]
  print $ diff'' [1,2,3] [2,3,4]

  print $ inter [1,2,3] [2,3,4]
  print $ inter' [1,2,3] [2,3,4]
  print $ inter'' [1,2,3] [2,3,4]
  print $ inter''' [1,2,3] [2,3,4]

  print $ uni [1,2,3] [2,3,4]
  print $ uni' [1,2,3] [2,3,4]
  print $ uni'' [1,2,3] [2,3,4]

diff :: Eq a => [a] -> [a] -> [a]
diff a b = (\\) a b

member :: Eq a => a -> [a] -> Bool
member a b = elem a b

notMember :: Eq a => a -> [a] -> Bool
notMember a b = notElem a b

diff' :: Eq a => [a] -> [a] -> [a]
diff' a b = filter ( (flip notMember) b ) a
{-- This one is close to the written notation for a difference set. eg A\B = [x| x E A, x \E B] --}
diff'' :: Eq a => [a] -> [a] -> [a]
diff'' a b = [x | x <- a, notMember x b]

inter :: Eq a => [a] -> [a] -> [a]
inter a b = intersect a b

inter' :: Eq a => [a] -> [a] -> [a]
inter' a b = [x | x <- a, member x b ]

inter'' :: Eq a => [a] -> [a] -> [a]
inter'' a b = filter ( (flip member) b ) a

inter''' :: Eq a => [a] -> [a] -> [a]
inter''' a b = fst $ partition (flip member b) a

uni :: Eq a => [a] -> [a] -> [a]
uni a b = union a b

-- Does not conform to haskell spec
uni' :: Eq a => [a] -> [a] -> [a]
uni' a b = nub $ a ++ b

uni'' :: Eq a => [a] -> [a] -> [a]
uni'' a b = a ++ (diff b a)


prop_associative_uni xs ys = (uni'' xs ys ) == (uni'' ys xs)
prop_commutative_uni xs ys zs = uni'' (uni'' xs ys ) zs == uni'' (uni'' xs zs) ys

prop_associative_diff xs ys = (uni'' xs ys ) == (uni'' ys xs)
prop_commutative_diff xs ys zs = uni'' (uni'' xs ys ) zs == uni'' (uni'' xs zs) ys

{-- 
associative

--}
{-- 
commutative

--}

{-- 
Identity

--}

  -- -- symClos [(1,2),(2,3), (3,4)]
  -- main = print  $ nub $  until (\x -> (nub $ trClos x) == (nub  x)) trClos  [(1,2),(2,3)]
  
  
-- =============================================================================
-- Exercise 4 :: Time spent +-
-- =============================================================================
exercise4 = do
  print()

-- =============================================================================
-- Exercise 5 :: Time spent +- 15
-- =============================================================================
exercise5 = do
  print()

type Rel a = [(a,a)]

swap :: Ord a => (a, a) -> (a, a)
swap (a, b) = (b, a)

swapped :: Ord a => (a, a) -> [(a, a)]
swapped (a, b) = [(a, b), (b, a)]
{-- 
I solved the symClos in different ways. Union of the flipped values using either list comprehension or map values with a swap function. These methods are the
closest to the way you would think about the problem or write it down. (OriginalTupleList U FlippedTupleList). 
The third method a implementation focused solution. Make a function that returns a list of original and swapped values and concatMap the original list with that function.
--}
symClos :: Ord a =>  Rel a -> Rel a
symClos a =  sort $ uni a ([(y,x)| (x,y) <- a])

symClos' :: Ord a =>  Rel a -> Rel a
symClos' a = sort $ (uni a) $ map swap a

-- nub for good measure?
symClos'' :: Ord a =>  Rel a -> Rel a
symClos'' a = concatMap swapped a
    
-- =============================================================================
-- Exercise 6 :: Time spent +- 30 min
-- =============================================================================
exercise6 = do
  print()

-- TODO Print out example

infixr 5 @@
 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

transited :: Ord a => [(a, a)] ->(a, a) -> [(a, a)]
transited r  (a, c) = (a, c)  : [(a, y)| (x, y) <- r, c == x]

trClos' :: Ord a => Rel a -> Rel a
trClos' r = nub $ concatMap (transited r ) r

-- Does not work for 4,1 ....
-- trClos'' :: Ord a => Rel a -> Rel a
-- trClos'' r = nub $ (r @@ r)

trClos :: Ord a => Rel a -> Rel a
trClos = nub . until (\x -> (nub $ trClos' x) == (nub  x)) trClos'

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
