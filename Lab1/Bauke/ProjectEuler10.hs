-- Find the sum of all the primes below two million.
-- Too slow ... => Use Sieve? or default module?
-- Took implementation of WJ

main = print sumOfPrimes

sumOfPrimes :: Integer
sumOfPrimes = sum $ takeWhile (< 2000000) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes