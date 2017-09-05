module Exercise1 where
import Data.List
import Test.QuickCheck
import Control.Monad

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

solution10 :: Integer
solution10 = sum ( takeWhile (< 2000000) primes )

solution9 :: Integer
solution9 = head [a * b * c | a <- [1..1000], b <- [a..1000], let c = 1000 - a -b, a^2 + b^2 == c^2]
