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

main = findSum 0 101

findSum :: Int -> Int -> [Int]
findSum start amount | elem (sum $ primeList start amount) primes =  primeList start amount
                     | otherwise = findSum (start+1) amount

primeList :: Int -> Int -> [Int]
primeList a b = drop a $ take (a+b) primes

primes :: [Int]
primes = 2 : filter prime [3..]

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes