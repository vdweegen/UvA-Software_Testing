module Exercise5 where

import Data.List
import Test.QuickCheck

-- Implementation time: 30 minutes
-- It's way too slow. it will find for 2, but certainly not for 101.
-- 5 primes takes ages
-- main issue is probably the drop & take, it re-generates the list one every iteration

-- While the iteration is performed forwards, testing it would result in performing the same operation again.
-- All combinations could be checked, but this is exactly what the algorithm does.
-- If the result matches the new prime, it is valid

-- Whilst trying to optimize I noticed i forgot the termination in the elem call. So any non-prime number (4) would cause
-- the elem .. primes to run forever.
-- Fixed this by introducing primesTill ..., which returns the list up and including the argument passed

main = sum $ findConsecutive 101

findConsecutive :: Int -> [Int]
findConsecutive size = findSum size size (sum $ primeList 0 size)

findSum :: Int -> Int -> Int -> [Int]
findSum size nextIndex actualValue | elem actualValue (primesTill actualValue) = primeList (nextIndex-size) size
                                   | otherwise = findSum size (nextIndex+1) (updateSum size nextIndex actualValue)

primesTill :: Int -> [Int]
primesTill a = takeWhile(<= a) primes

primeList :: Int -> Int -> [Int]
primeList a b = drop a $ take (a+b) primes

updateSum :: Int -> Int -> Int -> Int
updateSum size index sumValue = sumValue + (primes !! index) - (primes !! (index - size))

primes :: [Int]
primes = 2 : filter prime [3..]

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes