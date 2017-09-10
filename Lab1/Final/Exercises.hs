module Lab1 where
import Data.List
import Test.QuickCheck

-- Assignment 1 / Lab 1 :: Group 14 --

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Define Main --
main = do
    putStrLn $ "===================="
    putStrLn $ "Assignment 1 / Lab 1"
    putStrLn $ "===================="
    putStrLn $ "Exercise 1.1"
    exercise1_1
    putStrLn $ "Exercise 1.2"
    exercise1_2
    putStrLn $ "Exercise 2"
    -- exercise2
    putStrLn $ "Exercise 3"
    -- exercise3
    putStrLn $ "Exercise 4"
    -- exercise4
    putStrLn $ "Exercise 5"
    -- exercise5
    putStrLn $ "Exercise 6"
    -- exercise6
    putStrLn $ "Exercise 7"
    -- exercise7
    putStrLn $ "Exercise 8"
    -- exercise8
    putStrLn $ "BONUS"
    -- TODO

-- Exercise 1.1
basecase1 :: Integer -> Integer
inductioncase1 :: Integer -> Integer
basecase1 n = sum [ a^2 | a <- [0..n]]
inductioncase1 n = (n*(n+1)*(2*n+1)) `div` 6
exercise1_1 = quickCheckResult(\n -> n >= 0 --> basecase1 n == inductioncase1 n)

-- Exercise 1.2
basecase2 :: Integer -> Integer
inductioncase2 :: Integer -> Integer
basecase2 n = sum [ a^3 | a <- [0..n]]
inductioncase2 n = ((n*(n+1)) `div` 2 ) ^ 2
exercise1_2 = quickCheckResult(\n -> n >= 0 --> basecase2 n == inductioncase2 n)

-- Exercise 2

-- Exercise 3

-- Exercise 4

-- Exercise 5

-- Exercise 6

-- Exercise 7

-- Exercise 8
