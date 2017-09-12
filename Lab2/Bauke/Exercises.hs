module Exercises where
import Data.List

import Lab2.Util.Random
import Lab2.Util.Infix

-- Define Main --
main = do
    putStrLn $ "===================="
    putStrLn $ "Assignment 2 / Lab 2"
    putStrLn $ "===================="
    putStrLn $ "> Exercise 1"
    exercise1
    putStrLn $ "> Exercise 2"
    exercise2
    putStrLn $ "> Exercise 3a"
    exercise3a
    putStrLn $ "> Exercise 3b"
    exercise3b
    putStrLn $ "> Exercise 4"
    exercise4
    putStrLn $ "> Exercise 5"
    exercise5
    putStrLn $ "> Exercise 6"
    exercise6
    putStrLn $ "> Exercise 7"
    exercise7
    putStrLn $ "> BONUS"
    exercisebonus


-- Exercise 1 : Checking random function
-- Implementation time: 60 minutes
-- This took very long due to the IO stuff. We handled it in the summer school, but it took some time to figure out anyway.
-- So far, i can SEE that the distribution is correct, but the next step is to also SHOW that is is correct.

exercise1 =  probs 10000 >>= (\list -> return $ isEvenlyDistributed list)

isEvenlyDistributed :: [Float] -> (Integer, Integer, Integer, Integer)
isEvenlyDistributed = sumTuples . categorize

categorize :: [Float] -> [(Integer, Integer, Integer, Integer)]
categorize fs = [ b | a <- fs, let b = createTuple a]

createTuple :: Float -> (Integer, Integer, Integer, Integer)
createTuple float | float < 0.25 = (1,0,0,0)
                  | float < 0.50 = (0,1,0,0)
                  | float < 0.75 = (0,0,1,0)
                  | otherwise = (0,0,0,1)

sumTuples :: [(Integer, Integer, Integer, Integer)] -> (Integer,Integer,Integer,Integer)
sumTuples xs = doSum xs (0,0,0,0)

doSum :: [(Integer, Integer, Integer, Integer)] -> (Integer,Integer,Integer,Integer) -> (Integer,Integer,Integer,Integer)
doSum [] a = a
doSum ((a1,a2,a3,a4):xs) (b1,b2,b3,b4) = doSum xs (a1+b1, a2+b2, a3+b3, a4+b4)

-- Exercise 2
-- Implementation finished in 10 minutes, without the tests
-- Simply keying in the definitions for the triangles
-- The pythagorean algorithm can probably be refactored by sorting a b c and then taking 2 and comparing against last
-- @ todo => add the properties to be tested
data Shape = NoTriangle | EquiLateral | Isosceles | Rectangular | Other
             deriving (Eq, Show)

exercise2 = sampleTriangles

sampleTriangles = do
              print $ triangle 60 80 100 -- default Rectangular used in construction
              print $ triangle 10 10 10  -- Equilateral
              print $ triangle 1 1 100 -- Nothing
              print $ triangle 10 10 9 -- IsoSceles
              print $ triangle 10 9 8 -- Something else

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | not $ validTriangle a b c = NoTriangle
               | (a == b) && (a == c) = EquiLateral
               | (maximum [a,b,c])^2 == (c^2) + (a^2) + (b^2) - (maximum [a,b,c])^2 = Rectangular
               | (a == b ) || (a == c) || (b == c) = Isosceles -- "Understandable" implementation
     --          | 2 == (length $ fromList [a,b,c]) = Isosceles -- "Smart" implementation
               | otherwise = Other

validTriangle :: Integer -> Integer -> Integer -> Bool
validTriangle a b c = (a + b > c) && (a + c > b) && (b + c > a)

-- Exercise 3a
-- Implementation time: 15 minutes
-- Writing the generic function took most time,
-- Afterwards i noticed that haskell provides `isSubSet`

-- Exercise 3b
-- Implementation time: 20 minutes
-- Checked on the usage custom sort algorithm. Haskell provides sortBy.
-- Implemented the Ordering operator for the lists and passed this to the sort
-- Note that this ordering assumes that all sets are contained in the powerset.
-- If neither set is stronger, the order will remain unchanged.

data PropertyStrength = Left_Stronger | Right_Stronger | Both_Stronger | Invalid_Comparison
  deriving (Eq, Show)

-- Implement the properties and show which one is stronger
exercise3a = do
              putStr "'(\\ x -> even x && x > 3)' compared to 'even': "
              print $ compareProperties prop3aList evenList
              putStr "'(\\ x -> even x || x > 3)' compared to 'even': "
              print $ compareProperties prop3bList evenList
              putStr "'(\\ x -> (even x && x > 3) || even x)' compared to 'even': "
              print $ compareProperties prop3cList evenList
              putStr "'even' compared to '(\\ x -> (even x && x > 3) || even x)': "
              print $ compareProperties evenList prop3cList

exercise3b = print $ sortBy compareSets [prop3aList, prop3bList, prop3cList, evenList]

prop3aList :: [Integer]
prop3aList = filter (>3) evenList

prop3bList :: [Integer]
prop3bList = sort $ asSet $ merge largerThan3 evenList

prop3cList :: [Integer]
prop3cList = sort $ asSet $ merge prop3aList evenList

largerThan3 :: [Integer]
largerThan3 = filter (>3) domain

evenList :: [Integer]
evenList = filter even domain

compareProperties :: [Integer] -> [Integer] -> PropertyStrength
compareProperties lhs rhs | isSubset lhs rhs && isSubset rhs lhs = Both_Stronger
                          | isSubset lhs rhs = Left_Stronger
                          | isSubset rhs lhs = Right_Stronger
                          | otherwise = Invalid_Comparison

compareSets :: [Integer] -> [Integer] -> Ordering
compareSets xs ys | isSubset xs ys = LT
                  | isSubset ys xs = GT
                  | otherwise = EQ

domain :: [Integer]
domain = [-10..10]

merge :: [Integer] -> [Integer] -> [Integer]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- Exercise 4
exercise4 = print $ isPermutation [1,2,3,4] [4,2,3,1]

-- implementation of perms using the earlier provided isSubset method
-- note, as specified in the assignment, it does not process lists with dupes dupes.
-- If the dupes were important, one could simply count occurrences and compare this.
-- if xs /= ys and sort xs == sort ys, then it's a permutation.
-- For the testing procedure, it means you have to remove any duplicate from the input lists
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys | xs == ys = False
                    | (asSet xs) /= xs = False
                    | (asSet ys) /= ys = False
                    | otherwise = isSubset xs ys && isSubset ys xs

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset [] set = True
isSubset (x:xs) set = elem x set && isSubset xs set

asSet :: Eq a => [a] -> [a]
asSet [] = []
asSet (x:xs) | elem x xs = asSet xs
             | otherwise = x : asSet xs

-- Exercise 5
exercise5 = print()

-- Exercise 6
exercise6 = print()

-- Exercise 7
exercise7 = print()

-- Bonus Exercises
exercisebonus = print()
