module Lab6 where

import Lecture6
import Formatting
import Formatting.Clock
import Numeric

import Control.Monad

import Data.Bits
import Data.Char
import Data.List

import System.Clock
import System.Random

import Test.QuickCheck

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
-- Then fixed the implementation using the 'div' method.
-- When merging final solutions, some used the shiftR which is faster than div
-- Modified the solution. The implementation is shown here as exM', equal to exM in the lecture
-- To verify the results over a larger range, we used quickCheck to generate some examples
-- =============================================================================

exercise1 = do
  putStrLn $ "Checking example. 3^200 mod 50: " ++ show (exM 3 200 50)
  putStrLn $ "Compare expM and exM. result equal: " ++ show ((exM 3 200 50) == (expM 3 200 50))
  putStrLn "Generating arbitrary amount, using quickCheck"
  quickCheck prop_exm

-- | Copied implementation from exM in the lecture.hs
exM' :: Integer -> Integer -> Integer -> Integer
exM' b 1 m = b `mod` m
exM' b e m | even e = squaredMod 1
           | odd e = squaredMod b
            where squaredMod v = v * exM' b (e `shiftR` 1) m ^ 2 `mod` m

prop_exm :: (Positive Integer, Positive Integer, Positive Integer) -> Bool
prop_exm (Positive b, Positive e, Positive m) = exM' b e m == expM b e m

-- =============================================================================
-- Exercise 2 :: Time spent: +- 2 hours
-- A fair test should apply the same inputs to each function
-- It first generate 3 list of input and use them with both functions
-- Our own implementation is roughly twice as fast as the standard implementation.
-- A slightly faster implementation is available from the Crypto.Numbers package.
-- See exercises of Jordan Maduro
-- =============================================================================
type Interval = (TimeSpec, TimeSpec)

exercise2 = do
    bs <- randomValues
    es <- randomValues
    ms <- randomValues
    ownImplementation <- profile (doCalculation' exM bs es ms)
    standardImplementation <- profile (doCalculation' expM bs es ms)
    reportTime "optimized variant" ownImplementation
    reportTime "standard implementation" standardImplementation

reportTime :: String -> Interval -> IO ()
reportTime str (start,end) = do
  fprint timeSpecs start end
  putStrLn $ " when using the " ++ str

profile :: Show a => a -> IO Interval
profile f = do
  start <- getTime Monotonic
  print f
  end <- getTime Monotonic
  return (start,end)

doCalculation' :: (Integer -> Integer -> Integer -> Integer) -> [Integer] -> [Integer] -> [ Integer] ->[[Integer]]
doCalculation' fn bs es ms = do
  let z = zip3 bs es ms
  let ys = map runFn z
  return ys
  where
    runFn (b, e , m) = fn b e m

randomValues :: IO [Integer]
randomValues = replicateM 10 randomInt

randomInt :: IO Integer
randomInt = do
    randomRIO (400, 10000 :: Integer)

-- =============================================================================
-- Exercise 3 :: Time spent: +- 5 minutes
-- Simply write a list comprehension containing all non primes
-- Checked the implementation against the wikipedia link for the known composites up to 150.
-- These are exactly equal
-- Again, composites' is posted here as a copy from the lecture
-- =============================================================================

exercise3 = do
  putStr "Checking composites against known values up to 150: "
  print verifyComposites

composites' :: [Integer]
composites' = [ a | a <- [3..], not $ prime a]

verifyComposites :: Bool
verifyComposites = takeWhile (<=150) composites == firstComposites

firstComposites :: [Integer]
firstComposites = [4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20, 21, 22, 24, 25, 26, 27, 28,
                   30, 32, 33, 34, 35, 36, 38, 39, 40, 42, 44, 45, 46, 48, 49, 50, 51,
                   52, 54, 55, 56, 57, 58, 60, 62, 63, 64, 65, 66, 68, 69, 70, 72, 74,
                   75, 76, 77, 78, 80, 81, 82, 84, 85, 86, 87, 88, 90, 91, 92, 93, 94,
                   95, 96, 98, 99, 100, 102, 104, 105, 106, 108, 110, 111, 112, 114,
                   115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 128,
                   129, 130, 132, 133, 134, 135, 136, 138, 140, 141, 142, 143, 144,
                   145, 146, 147, 148, 150]

-- =============================================================================
-- Exercise 4 :: Time spent: +- 1 hour
-- The smallest composite number that passes the test is 9
-- If k = 1 it runs fast if k = 2 then takes a little longer but comes to the same conclusion. Running k = 5
-- takes a lot longer and got as low as 15 in one test.
-- When increasing the k-value, the algorithm gets less false positives.
-- The value of the found values decreases, as can be seen from the calculated average 'prime' value
-- =============================================================================
exercise4 = do
  k1 <- testFer 10 (testFermatKn 1)
  k2 <- testFer 10 (testFermatKn 2)
  k5 <- testFer 10 (testFermatKn 5)

  putStrLn " Exercise 4: Smallest composite number that passes Fermat test"
  report 1 k1
  report 2 k2
  report 5 k5

report :: Integer -> (Integer, Integer) -> IO()
report n (min,avg) = putStrLn $ "K = " ++ show n ++ ", minimum 'prime': " ++ show min ++ " can be divided by " ++ show (take 3 $ dividers min) ++ ", average value of primes found: " ++ show avg

testFer :: Int -> IO Integer -> IO (Integer, Integer)
testFer n x = do
    x <- replicateM n x
    small <- testFerSmall x
    avg <-  testFerAvg x
    return (small , avg)

testFerAvg, testFerSmall :: [Integer] -> IO Integer
testFerAvg x = do
  let avg = sum x `div` genericLength x
  return avg

testFerSmall x = do
  let sorted = sort x
  return $ head sorted

testFermatKn :: Int -> IO Integer
testFermatKn n = foolFermat' n composites

foolFermat' :: Int -> [Integer] -> IO Integer
foolFermat' k (x:xs) = do
    z <- primeTestsF k x
    if z then
      return x
    else
      foolFermat' k xs

-- | an empty list is returned for every prime, otherwise the list of dividers
dividers :: Integer -> [Integer]
dividers n = [ a | a <- [2..n-1], n `rem` a == 0 ]


-- =============================================================================
-- Exercise 5 :: Time spent: +- 2 hours
-- This function uses J. Chernick's theorem to construct a subset of carmichael numbers.
-- The fermat test is easily by the first 2 numbers produced by the carmichael function
-- =============================================================================
exercise5 = do
  k1 <- testFer 5 (testFermatCarmichaelKn 1)
  k2 <- testFer 5 (testFermatCarmichaelKn 2)
  k5 <- testFer 5 (testFermatCarmichaelKn 3)
  putStrLn " Exercise 5: Smallest number in J. Chernick's subset of carmichael numbers that passes Fermat test"
  report 1 k1
  report 2 k2
  report 5 k5

testFermatCarmichaelKn n = foolFermat' n carmichael

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
          k <- [2..],
          prime (6*k+1),
          prime (12*k+1),
          prime (18*k+1) ]

-- =============================================================================
-- Exercise 6 (1) :: Time spent: +- 30 min
-- The numbers are much larger but the Miller-Rabin primality check does get fooled.
-- When using K factor 5, no counter examples were found, even running for very long periods of time.
-- =============================================================================
exercise6 = do
  k1 <- testFer 1 (testMRKn 1)
  k2 <- testFer 1 (testMRKn 2)
  k5 <- testFer 1 (testMRKn 3) -- | NO counter values for K = 5
  putStrLn " Exercise 6: Smallest number in J. Chernick's subset of carmichael numbers that passes Miller-Rabin"
  report 1 k1
  report 2 k2
  report 5 k5

testMRKn n = testMR n carmichael

testMR :: Int -> [Integer] -> IO Integer
testMR k (x:xs) = do
  z <- primeMR k x
  if z then
    return x
  else
    testMR k xs


-- =============================================================================
-- Exercise 6 (2) :: Time spent: +- 1 hour
-- Finding a list of mersenneprimes
-- Using the miller rabin primes, this is much faster than the algorithm using the normal prime
-- =============================================================================
exercise62 = do
 putStr "Comparing first 10 mersenne primes: "
 calculatedPrimes <- filterM (primeMR 1.(\x -> (2^x) - 1)) $ take 150 primes
 let primeValues = map mersenne calculatedPrimes
 print $ take 10 primeValues == take 10 knownMersennePrimes
 putStrLn ""

-- | Known mersenne Numbers
knownMersennePrimes :: [Integer]
knownMersennePrimes = [ mers n | n <- [1..25]]

mersenne :: Integer -> Integer
mersenne = subtract 1 . (2^)
-- =============================================================================
-- Exercise 7 :: Time spent: +- 30 minutes on large prime generator
-- Additional 2 hours on implementing and refactoring.
-- First I wrote down all methods myself, to completely understand how it's working.
-- Then refactored out the methods, replacing them by the ones provided in the lecture code
-- How does it work:
-- Encode a message to a single integer
-- Find a (large prime pair) with equal bit size as this integer
-- Anyone can encode the data using the public key, but only the keeper of the private key can decrypt
-- When the single integer is encoded, the encoded value is sent to the private key keeper
-- Only the recipient, knowing the private key, can decrypt this back to the original information.
-- =============================================================================
exercise7 = do
  let message = "Hello, World!"
  putStrLn $"Encrypting a message: " ++ message
  encryptionExample message

-- | composes a message in one hexadeximal value
composeMessage :: String -> Integer
composeMessage msg = read $ "0x" ++ (concat [ showHex (ord a) ""| a <- msg]) :: Integer

-- | decomposes message back to string
decomposeMessage :: Integer -> String
decomposeMessage n = convert (showHex n "")

-- | Convert hex number to Ascii String
convert :: String -> String
convert [] = []
convert (x1:x2:xs) = (chr $ (read ("0x" ++ [x1] ++ [x2]) :: Int)) : convert xs

-- | Encrypt and decrypt a message
encryptionExample :: String -> IO()
encryptionExample str = do
  let inputNumber = composeMessage str
  putStrLn $ "Composed message to single hex number: " ++ show inputNumber
  (p,q) <- largePrimePair $ Lab6.bitSize inputNumber
  let encrypted = rsaEncode (rsaPublic p q) inputNumber
  putStrLn $ "Encrypted message: " ++ show encrypted
  let decrypted = rsaDecode (rsaPrivate p q) encrypted
  putStrLn $ "Received an encrypted message, decoding results in: " ++ show decrypted
  putStrLn $ "Composed back to ASCII:" ++ decomposeMessage decrypted

-- | returns a large prime pair based on the bit size
largePrimePair :: Integer -> IO (Integer,Integer)
largePrimePair n = do
  a <- findPrime (2^n)
  b <- findPrime (a+1)
  return (a,b)

-- | Bit size of an integer
bitSize :: Integer -> Integer
bitSize = genericLength . int2bin

-- | Compose integer as list of bits [LSB .. MSB ]
int2bin :: Integer -> [Integer]
int2bin 0 = []
int2bin n = mod n 2 : int2bin (shiftR n 1)

-- | Given a start value, finds the closest prime above
findPrime :: Integer -> IO Integer
findPrime val = do
  prime <- primeMR 5 val
  if prime then
    return val
  else
    findPrime (val+1)
