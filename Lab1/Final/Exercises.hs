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
    exercise2
    putStrLn $ "Exercise 3"
    exercise3
    putStrLn $ "Exercise 4"
    exercise4
    putStrLn $ "Exercise 5"
    exercise5
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
prop_subsequenceSize :: [Integer] -> Bool
prop_subsequenceSize n =
  (^) 2 (genericLength n) == genericLength (subsequences n)
exercise2 = quickCheckWith stdArgs { maxSize = 25 } prop_subsequenceSize

-- Exercise 3
factorial n  = product [1..n]
solution3 (Positive n) = (length $ permutations [1..n]) == factorial(n)
exercise3 = quickCheckWith stdArgs { maxSize=10 } solution3

-- Exercise 4
reversal :: Integer -> Integer
reversal = read . reverse . show

-- helper method to check if reversal is also prime
primeReverse :: Integer -> Bool
primeReverse n = prime n && prime (reversal n)

-- simply filter the list
-- solution4a :: [Integer]
-- solution4a = filter primeReverse [0..9999]

-- bit more efficient, instead of first generating the entire list,
-- now only create a list with the correct values on the fly
solution4b :: [Integer]
solution4b = [a | a <- [1..9999], primeReverse a]

exercise4 = do
  print $ solution4b

-- Exercise 5

-- It's way too slow. it will find for 2, but certainly not for 101.
-- 5 primes takes ages
-- main issue is probably the drop & take, it re-generates the list one every iteration

-- While the iteration is performed forwards, testing it would result in performing the same operation again.
-- All combinations could be checked, but this is exactly what the algorithm does.
-- If the result matches the new prime, it is valid

-- Whilst trying to optimize I noticed i forgot the termination in the elem call. So any non-prime number (4) would cause
-- the elem .. primes to run forever.
-- Fixed this by introducing primesTill ..., which returns the list up and including the argument passed

-- How to test is your answer is correct:
-- Take the list of all primes up to the maximum prime value in the solution list
-- Compose all lists of 101 primes from this list
-- Validate that NO sum of these lists is a prime

primesSum x y = sum $ take x $ drop y primes
primesconsum =  head $ filter (prime) $ map (primesSum 101) $ [0..]

exercise5 = do
    print $ primesconsum
-- Exercise 6

-- Exercise 7

-- Exercise 8
