module Exercise5 where

import Data.List

-- Implementation time: 5 minutes
-- Reason => solutions to Euler questions nearly has the items in place
-- Tests which can be performed:

-- All items in the tuples should be a prime
-- The size of the list should be smaller than the list of primes
-- size of the items in the tuples should be equal
-- Value of items should be different

main = print [ (a, (reversal a)) | a <- validPrimes, elem (reversal a) validPrimes, a < (reversal a)]

reversal :: Integer -> Integer
reversal = read . reverse . show

validPrimes :: [Integer]
validPrimes = takeWhile (<10000) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes