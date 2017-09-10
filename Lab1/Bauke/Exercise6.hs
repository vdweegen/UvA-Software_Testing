import Data.List

-- Implementation time: 30 minutes
-- List comprehension took some time.
-- the Let introduces the ability to use local scoped variables.

main = print $ head listOfCounters

listOfCounters :: [[Int]]
listOfCounters = [ take a primes | a <- [1..], let xs = take a primes, not $ prime $ 1 + product xs ]

primes :: [Int]
primes = 2 : filter prime [3..]

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes