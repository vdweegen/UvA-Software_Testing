import Data.List

-- Implementation time: 30 minutes
-- List comprehension took some time.
-- the Let introduces the ability to use local scoped variables.

main = print $ take 1 listOfCounters

listOfCounters :: [[Int]]
listOfCounters = [ take a primes | a <- [1..], let xs = take a primes, False == (prime $ calculateAssumedPrime xs) ]

calculateAssumedPrime :: [Int] -> Int
calculateAssumedPrime xs = (product xs) + 1

primes :: [Int]
primes = 2 : filter prime [3..]

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes