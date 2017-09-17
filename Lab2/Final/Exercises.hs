module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Assignment 2 / Lab 2 :: Group 14 --
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all


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
exercise3a = solution3a

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

one, two, three, four :: Int -> Bool
one = (\x -> even x && x > 3)
two = (\x -> even x || x > 3)
three = (\x -> (even x && x > 3) || even x)
four = (\x -> (even x && x > 3) || even x)

domain :: [Int]
domain = [-10..10]

data PropertyStrength = Stronger | Weaker | Equivalent | Incomparable
  deriving (Eq, Show)

compar :: [a] -> (a -> Bool) -> (a -> Bool) -> PropertyStrength
compar xs p q
  | (stronger xs p q) && (stronger xs q p) = Equivalent
  | stronger xs p q = Stronger
  | stronger xs q p = Weaker
  | otherwise = Incomparable

combcompar x y = compar x (y !! 0) (y !! 1)

instance Ord PropertyStrength where
  compare Stronger Stronger = EQ
  compare Stronger Weaker = GT
  compare Stronger Equivalent = GT
  compare Stronger Incomparable = GT
  compare Equivalent Stronger = LT
  compare Equivalent Equivalent = EQ
  compare Equivalent Weaker = GT
  compare Equivalent Incomparable = GT
  compare Weaker Stronger = LT
  compare Weaker Equivalent = LT
  compare Weaker Weaker = EQ
  compare Weaker Incomparable = GT
  compare Incomparable Stronger = LT
  compare Incomparable Equivalent = LT
  compare Incomparable Weaker = LT
  compare Incomparable Incomparable = EQ

solution3a = do
  print $ compar domain one two
  print $ compar domain one three
  print $ compar domain one four
  print $ compar domain two three
  print $ compar domain three four

-- Exercise 3b
exercise3b = solution3b

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

solution3b = do
  print $ sort $ map (combcompar domain) (combinations 2 [one,two,three,four])

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
