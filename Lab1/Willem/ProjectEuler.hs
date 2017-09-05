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
