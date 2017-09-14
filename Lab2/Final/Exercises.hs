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
-- QuickCheck for generating a number of random values and checking that the value is maintained
exercise1 = quantilesIO 10000 4

quantilesIO :: Int -> Int -> IO()
quantilesIO xs q = do {
    p <- probs xs;
    print $ flip chi (div xs q) $ quantiles p [ fromIntegral x / (fromIntegral q) | x  <- [1..q]]
}

chi :: [Int] -> Int -> Float
chi [] m = 0
chi (x:xs) m = (fromIntegral((x-m)^2) / fromIntegral m) + chi xs m

quantiles :: [Float] -> [Float] -> [Int]
quantiles xs [] = []
quantiles xs (q:qs) = [genericLength $ filter (<q) xs] ++ (quantiles (filter (>=q) xs) qs)


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
