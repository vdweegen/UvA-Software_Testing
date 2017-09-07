module Lab1 where
import Data.List
import Test.QuickCheck

-- Lab Week 1 --

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
    putStrLn $ "Assignment 1.1"
    result1_1
    putStrLn $ "Assignment 1.2"
    result1_2
    putStrLn $ "Assignment 2"
    result2
    putStrLn $ "Assignment 3"
    -- TODO
    putStrLn $ "Assignment 4"
    -- TODO
    putStrLn $ "Assignment 5"
    -- TODO
    putStrLn $ "Assignment 6"
    -- TODO
    putStrLn $ "Assignment 7"
    -- TODO
    putStrLn $ "Assignment 8"
    -- TODO
    putStrLn $ "BONUS"
    -- TODO

-- Assignment 1.1 --
base1_1 :: Integer -> Integer
induction1_1 :: Integer -> Integer
base1_1 = \n -> sum(map(^2)[0..n])
induction1_1 = \n -> (n*(n+1)*(2*n+1)) `div` 6
result1_1 = quickCheckResult(\n -> n >= 0 --> base1_1 n == induction1_1 n)

-- Assignment 1.2 --
base1_2 :: Integer -> Integer
induction1_2 :: Integer -> Integer
base1_2 = \n -> sum(map(^3)[0..n])
induction1_2 = \n -> ((n*(n+1)) `div` 2 ) ^ 2
result1_2 = quickCheckResult(\n -> n >= 0 --> base1_2 n == induction1_2 n)

-- Assignment 2 --
base2 :: Int -> Int
induction2 :: Int -> Int
base2 = \n -> 2^n
induction2 = \n -> length(subsequences[1..n])
result2 = quickCheckResult(\n -> n >= 0 --> induction2 n == base2 n)

-- Assignment 3 --

-- Assignment 4 --

-- Assignment 5 --

-- Assignment 6 --

-- Assignment 7 --

-- Assignment 8 --
