module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

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
    -- exercise1
    putStrLn $ "> Exercise 2"
    -- exercise2
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

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n-1)
  return (p:ps)

exercise1 = quantilesIO 10000 4

quantilesIO :: Int -> Int -> IO()
quantilesIO xs q = do {
    p <- probs xs;
    print $ flip chi (div xs q) $ quantiles p [ fromIntegral x / (fromIntegral q) | x  <- [1..q]]
}

-- Using a chi squared test to verify if the bins are evenly distributed
-- https://en.wikipedia.org/wiki/Chi-squared_test
-- https://www.medcalc.org/manual/chi-square-table.php
-- Using (n-1) = (4-1) = 3 degrees a freedom and a P value of 0.05
-- The result of the chi should not exceed 7.815
-- In this case it doesn't exceed that value, so we can say the random generator is good,
-- based on the fact of dividing them into 4 bins, however we cannot see anything about the distribution within a bin.
chi :: [Int] -> Int -> Float
chi [] m = 0
chi (x:xs) m = (fromIntegral((x-m)^2) / fromIntegral m) + chi xs m

quantiles :: [Float] -> [Float] -> [Int]
quantiles xs [] = []
quantiles xs (q:qs) = [genericLength $ filter (<q) xs] ++ (quantiles (filter (>=q) xs) qs)

-- Exercise 2


data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

exercise2 = do
              quickCheck prop_Equilateral
              quickCheck prop_Isosceles
              quickCheck prop_Invalid

prop_Equilateral (Positive n) =
  Equilateral == triangle n n n

prop_Isosceles (Positive n) =
  True == ((Isosceles == (triangle n n (randBetween 1 (2*n))))
          && (Isosceles == (triangle n (randBetween 1 (2*n)) n))
          && (Isosceles == (triangle (randBetween 1 (2*n)) n n)))

prop_Invalid (Positive n) (Positive o) =
  True == (NoTriangle == triangle n o (randAbove (n+o)))

-- Not random, but just to fix quickCheck
randBetween :: Integer -> Integer -> Integer
randBetween a b = a + (div b 2)

randAbove :: Integer -> Integer
randAbove n = n + 10;

checkShape :: (Integer, Integer, Integer) -> Shape
checkShape (a,b,c) = triangle a b c

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | noTriangle a b c = NoTriangle
               | equilateral a b c = Equilateral
               | isosceles a b c = Isosceles
               | rectangular a b c = Rectangular
               | otherwise = Other

isTriangle, noTriangle, equilateral, isosceles, rectangular :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = (a + b > c) || (a + c > b) || (b + c > a)
noTriangle a b c = not $ isTriangle a b c
equilateral a b c = (a == b) && (a == c) && (b == c)
isosceles a b c = (a == b) || (a == c) || (b == c)
rectangular a b c = (a^2+b^2) == c^2 || (a^2+c^2) == b^2 || (b^2+c^2) == a^2

equilateralProp, isoscelesProp, rectangularProp :: Integer -> [(Integer, Integer, Integer)]
equilateralProp n = [(a,b,c)| a <- [1..n], b <- [1..n], c <- [1..n], a == b && b == c]
isoscelesProp n   = [(a,b,c)| a <- [1..n], b <- [1..n], c <- [1..n], (a == b) || (a == c) || (b == c)]
rectangularProp n = [(a,b,c)| a <- [1..n], b <- [1..n], c <- [1..n], (a^2+b^2) == c^2 || (a^2+c^2) == b^2 || (b^2+c^2) == a^2]

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
