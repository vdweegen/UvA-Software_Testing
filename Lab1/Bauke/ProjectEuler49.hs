-- Find the triples a,b,c, where distance between them is 3330
-- One example is given, find the other one.

-- Implementation time: 30 minutes
-- Reason: Fairly easy, still require some extra practice on the 'short hand' notation
-- Currently, all steps are splitted in separate function calls, the Haskell provides some native constructs.

import Data.List


main = print $ tail [ a | a <- possibleTuples, allPermutations a]

possibleTuples :: [(Integer, Integer, Integer)]
possibleTuples = [ (a,a+3330,a+6660) | a <- possiblePrimes, elem (a + 3330) possiblePrimes, elem (a + 6660) possiblePrimes]

possiblePrimes :: [Integer]
possiblePrimes = dropWhile(<1000) $ takeWhile (< 10000) primes

allPermutations :: (Integer, Integer, Integer) -> Bool
allPermutations (a,b,c) = (elem (split b) $ permutations (split a))
                        && (elem (split c) $ permutations (split a))

split :: Integer -> [Integer]
split a = [ div a 1000, div (mod a 1000) 100, div (mod a 100) 10, mod a 10 ]

primes :: [Integer]
primes = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes