module Exercise1 where
import Data.List
import Test.QuickCheck
import Control.Monad

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- Reusing the prime method from the lecture
-- Time spent: 30 min (on both)
-- 10: simply sum until 2.000.000

solution10 :: Integer
solution10 = sum ( takeWhile (< 2000000) primes )

-- 9: try to create a list and using guards to set the values and the constraints
-- next simply multiply them and get the first (and only) element

solution9 :: Integer
solution9 = head [a * b * c | a <- [1..1000], b <- [a..1000], let c = 1000 - a -b, a^2 + b^2 == c^2]

-- Reusing reversal from exercise
reversal :: Integer -> Integer
reversal = read . reverse . show

-- helper method to check if reversal is also prime
primeReverse :: Integer -> Bool
primeReverse n = prime n && prime (reversal n)

-- simply filter the list
solution5a :: [Integer]
solution5a = filter primeReverse [0..9999]

-- bit more efficient, instead of first generating the entire list,
-- now only create a list with the correct values on the fly
solution5b :: [Integer]
solution5b = [a | a <- [1..9999], primeReverse a]