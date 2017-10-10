module Lab6 where

import Bauke.Lecture6
import Data.List
import System.Clock
import System.Random

import System.IO.Unsafe (unsafeInterleaveIO)
-- Define Main --
main = do
    putStrLn "===================="
    putStrLn "Assignment 6 / Lab 6"
    putStrLn "===================="
    putStrLn "> Exercise 1"
    exercise1
    putStrLn "> Exercise 2"
    exercise2
    putStrLn "> Exercise 3"
    exercise3
    putStrLn "> Exercise 4"
    exercise4
    putStrLn "> Exercise 5"
    exercise5
    putStrLn "> Exercise 6 (1)"
    exercise6
    putStrLn "> Exercise 6 (2)"
    exercise62
    putStrLn "> Exercise 7 (BONUS)"
    exercise7

-- =============================================================================
-- Exercise 1 :: Time spent: +- 2 hours
-- First looked up the example on youtube.
-- Then tried to fix a solution, but somehow got stuck on the squaring
-- =============================================================================

exercise1 :: IO()
exercise1 = do
  putStrLn $ "checking example. 3^200 mod 50: " ++ (show $ exM 3 200 50)
  putStrLn $ "compare expM and exM. result equal: " ++ (show $ (exM 3 200 50) == (expM 3 200 50))

-- Sample 3 pow 200
-- First =< Compute first power => 3 mod 50 => 3
-- Second => sequre => 3^2 -> 9 mod 50 => 9
-- Third => 3^4 => 9 ^ 2 = 81 => 81 mod 50 => 31
-- Fouth => 3^8 => 31^2 mod 50 => 961 mod 50 = 11
-- Fifth => 3^16 => 11^2 mod 50 => 121 mod 50 => 21
-- Sixth => 3^32 => 21^2 mod 50 => 441 mod 50 => 41
-- Seventh => 3^64 -> 41^2 mod 50 => 1681 mod 50 = 31
-- Eight => 3^128 -> 31^2 mod 50 => 961 mod 50 = 11
-- IF Exceeds => STOP -> Substract, restart

-- =============================================================================
-- Exercise 2 :: Time spent: +- 30 minutes
-- Notice that using the same computation will result in skewed timing results.
-- The haskell compiler caches intermediate values and will print with faster results
-- Todo => Fix the implementation using the squared modulo.
-- Investigate how.
-- =============================================================================
exercise2 = do
  a <- randomRIO (1,5)
  b <- randomRIO (300,512)
  c <- randomRIO (10,50)
  putStrLn $ "Base: " ++ show a ++ ", Exp: " ++ show b ++ ", Mod: " ++ show c
  exMTime <- testTime $ calcExM (a,b,c)
  expMTime <- testTime $ calcExpM (a,b,c)
  exMTime' <- testTime $ calcExM (a,b,c)
  putStrLn $ "Processing with fast algorithm: " ++ (show exMTime)
  putStrLn $ "Processing with slow algorithm: " ++ (show expMTime)
  putStrLn $ "Re-processing with fast algorithm: " ++ (show exMTime')

calcExM :: (Integer, Integer, Integer) -> IO ()
calcExM (a,b,c) = do
  print $ exM a b c

calcExpM :: (Integer, Integer, Integer) -> IO ()
calcExpM (a,b,c) = do
  print $ expM a b c

-- | Profiler for a function
testTime :: IO a -> IO (TimeSpec)
testTime f = do
  start <- getTime Monotonic
  f
  end <- getTime Monotonic
  return (diffTimeSpec start end)

-- =============================================================================
-- Exercise 3 :: Time spent: +- 5 minutes
-- Simply write a list comprehension containing all non primes
-- Checked the implementation against the wikipedia link for the known composites up to 150.
-- These are exactly equal
-- =============================================================================
exercise3 = do
  putStr "Checking composites against known values up to 150: "
  print $ verifyComposites

verifyComposites :: Bool
verifyComposites = (takeWhile (<=150) composites) == firstComposites

firstComposites :: [Integer]
firstComposites = [4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20, 21, 22, 24, 25, 26, 27, 28, 30, 32, 33, 34, 35, 36, 38, 39, 40, 42, 44, 45, 46, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 60, 62, 63, 64, 65, 66, 68, 69, 70, 72, 74, 75, 76, 77, 78, 80, 81, 82, 84, 85, 86, 87, 88, 90, 91, 92, 93, 94, 95, 96, 98, 99, 100, 102, 104, 105, 106, 108, 110, 111, 112, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 128, 129, 130, 132, 133, 134, 135, 136, 138, 140, 141, 142, 143, 144, 145, 146, 147, 148, 150]
-- =============================================================================
-- Exercise 4 :: Time spent: +- 30 minutes
-- Took some time to generate a list of items from an infinite list.
-- Found an example only showing this using a filter.
-- When the k is increased, the value of the prime increases.
-- Funny thing is that the issues with the composited are always similar values
-- =============================================================================
exercise4 = do
  putStr "Smallest non-prime composite number k=[1..3]: "
  primes <- falsePrimes 3
  let falsePrime = head primes
  print falsePrime
  putStrLn ((show falsePrime) ++ " can be divided by " ++ (show $ dividers falsePrime))
  putStr "Smalles non-prime composite number k=[1..5]: "
  primes' <- falsePrimes 5
  let falsePrime' = head primes'
  print falsePrime'
  putStrLn ((show falsePrime') ++ " can be divided by " ++ (show $ dividers falsePrime'))

-- | Create a non-exhaustive list of false positives
falsePrimes :: Int -> IO [Integer]
falsePrimes n = filterMIO (primeTestsF n) composites

-- | Custom filter used to create monadic listst
filterMIO :: (a -> IO Bool) -> [a] -> IO [a]
filterMIO p = go
  where
    go []     = return []
    go (x:xs) = do
      xs' <- unsafeInterleaveIO (go xs)
      b   <- p x
      return $ if b then (x:xs') else xs'

-- | an empty list is returned for every prime, otherwise the list of dividers
dividers :: Integer -> [Integer]
dividers n = [ a | a <- [2..n-1], n `rem` a == 0 ]

-- =============================================================================
-- Exercise 5 :: Time spent: +- 10 minutes
-- Simply copy the list comprehension from the labs
-- Then feed it to exactly the same function as used in exercise 4
-- =============================================================================
exercise5 = do
  putStr "Some carmichael false positives: "
  carMichaels3 <- falseCarmichaels 3
  carMichaels5 <- falseCarmichaels 5
  print $ take 2 carMichaels3
  print $ take 2 carMichaels5

-- | Create a non-exhaustive list of carmichael false primes
falseCarmichaels :: Int -> IO [Integer]
falseCarmichaels n = filterMIO (primeTestsF n) carmichael

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1)
              | k <- [2..],
              prime (6*k+1),
              prime (12*k+1),
              prime (18*k+1) ]

-- =============================================================================
-- Exercise 6 (1) :: Time spent: +- 5 minutes
-- Again, the same algorithm to return the list of false positives.
-- However, this algorithm throws a 'Bus error: 10'.
-- Due to google this is caused by a large exponentiation.
-- Perhaps after correct implementation of 1, this issue is gone.
-- =============================================================================
exercise6 = do
  putStr "Some miller rabin tests, using carmichael numbers"
  millerRabin3 <- falseMillerRabin 3
  millerRabin5 <- falseMillerRabin 5
  print $ take 2 millerRabin3
  print $ take 2 millerRabin5

falseMillerRabin :: Int -> IO [Integer]
falseMillerRabin n = filterMIO (primeMR n) carmichael
-- =============================================================================
-- Exercise 6 (2) :: Time spent: +-
-- =============================================================================
exercise62 = do
  print()

-- =============================================================================
-- Exercise 7 :: Time spent: +-
-- =============================================================================
exercise7 = do
  print()
