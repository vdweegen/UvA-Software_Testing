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
exercise1 = solution1

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n-1)
  return (p:ps)

filterLower :: Float -> [Float] -> [Float]
filterHigher :: Float -> [Float] -> [Float]
filterLower n list = filter (\x -> x>n) list
filterHigher n list = filter (\x -> x<=n) list

solution1 = do
  numbers <- probs 10000
  let q1 = intersect (filterLower 0.0 numbers) (filterHigher 0.25 numbers)
  let q2 = intersect (filterLower 0.25 numbers) (filterHigher 0.50 numbers)
  let q3 = intersect (filterLower 0.50 numbers) (filterHigher 0.75 numbers)
  let q4 = intersect (filterLower 0.75 numbers) (filterHigher 1.00 numbers)
  print (length q1)
  print (length q2)
  print (length q3)
  print (length q4)


-- Exercise 2
exercise2 = solution2

data Shape = NoTriangle | Equilateral
  | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape

triangle x y z
  | x + y <= z || x + z <= y || y + z <= x = NoTriangle
  | x == y && y == z && z == y = Equilateral
  | x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || y^2 + z^2 == x^2 = Rectangular
  | x == y || x == z || y == z = Isosceles
  | otherwise = Other

solution2 = do
  print(triangle 1 1 1)
  print(triangle 1 2 2)
  print(triangle 3 4 5)
  print(triangle 1 3 3)
  print(triangle 1 1 0)

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

solution3a = do
  print $ compar domain one two
  print $ compar domain one three
  print $ compar domain one four
  print $ compar domain two three
  print $ compar domain three four

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
