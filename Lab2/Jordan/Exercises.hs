module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lab2.Util.Random
-- Assignment 2 / Lab 2 :: Group 14 --

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

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

-- Exercise 1
-- Seems to be evenly distributed  Time taken: about 1.5 to 2 hours

quantiles :: [Float] -> [Float] -> [Integer]
quantiles xs [] = []
quantiles xs (q:qs) = [genericLength $ filter (<q) xs] ++ (quantiles (filter (>=q) xs) qs)

quantilesIO xs q = do {
    p <- probs xs;
    print $ quantiles p [x/q | x  <- [1..q]]
}

exercise1 = quantilesIO 10000 4

-- Exercise 2
-- Estimated time taken 2 hours
-- Tried to think about the problem in an abstract way. Thus the most time was spent learning about triangles! XP
isTriangle = (all ((\xs -> head xs <= sum (tail xs)) )) . permutations

isRightTriangle abc = c == sum ab
    where abc2 = map(^2) abc
          c = maximum abc2
          ab = filter (<c) abc2
          
equalSides = length.group.sort

triangle x y z 
    | (not.isTriangle) abc =  NoTriangle
    | isRightTriangle abc = Rectangular
    | 2 == equalSides abc = Isosceles
    | 1 == equalSides abc = Equilateral
    | otherwise = Other
    where abc = [x,y,z]
    
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

exercise2 = print $ triangle 1 1 9

-- Exercise 3a
exercise3a = print()

-- Exercise 3b
exercise3b = print()

-- Exercise 4
exercise4 = print()

-- Exercise 5
exercise5 = print()

-- Exercise 6
exercise6 = print()

-- Exercise 7
exercise7 = print()

-- Bonus Exercises
exercisebonus = print()
