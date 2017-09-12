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
-- Seems to be evenly distributed

quintile :: [Float] -> [Float] -> [Integer]
quintile xs [] = []
quintile xs (q:qs) = [genericLength $ filter (<q) xs] ++ (quintile (filter (>=q) xs) qs)

quintileIO xs q = do {
    p <- probs xs;
    print $ quintile p [x/q | x  <- [1..q]]
}

exercise1 = quintileIO 10000 4

-- exercise1 = print $ foldl (\xs -> takeWhile (< (head [xs])) [0.1,0.2,0.5,0.6,0.9]

-- Exercise 2
exercise2 = print()

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
