module Exercise5 where

import Data.List
import Test.QuickCheck

main = print smallestConsecutive 5 101

--- Prime retriever
primes :: [Integer]
primes = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> :ry^2 <= n) primes