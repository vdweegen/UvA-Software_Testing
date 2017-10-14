module Lab6 where

import Control.Monad 
import Lecture6
import Data.List
import Data.Bits
import Control.Exception
import Data.Time
import System.Random
import Formatting
import Formatting.Clock
import System.Clock
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
-- Exercise 1 :: Time spent: +- 5 hours
-- While implementing multiple versions of the exM function I came acros a package
-- that implemented the function if the squaring method. 
-- =============================================================================

exercise1 = do
  print()

-- This the implimentation found in the crypto-numbers package -- Using exponentiation by squaring
exM' :: Integer -> Integer -> Integer -> Integer
exM' 0 0 m = 1 `mod` m
exM' b e m = loop e b 1
    where sq x          = (x * x) `mod` m
          loop 0 _  a = a `mod` m
          loop i s a = loop (i `shiftR` 1) (sq s) (if odd i then a * s else a)
  
-- =============================================================================
-- Exercise 2 :: Time spent: +- 2 hours
-- A fair test should apply the same inputs to each function
-- It first generate 3 list of input and use them with both functions
-- =============================================================================
exercise2 = do
    bs <- replicateM  1000000 (randomRIO (600, 10000 :: Integer))
    es <- replicateM  1000000 (randomRIO (600, 10000 :: Integer))
    ms <- replicateM  1000000 (randomRIO (600, 10000 :: Integer))
    start <- getTime Monotonic
    void (evaluate $ doCalculation' expM bs es ms )
    end <- getTime Monotonic
    fprint (timeSpecs) start end


doCalculation = do
  b <- randomRIO (1, 10000)
  e <- randomRIO (1, 10000)
  m <- randomRIO (1, 10000)
  evaluate (exM b e m)


doCalculation' :: (Integer -> Integer -> Integer -> Integer) -> [Integer] -> [Integer] -> [ Integer] ->[[Integer]]
doCalculation' fn bs es ms = do
  let z = zip3 bs es ms
  let ys = map (runFn) z
  return ys
  where 
    runFn (b, e , m) = fn b e m
  

randomInt = do
    x <- randomRIO (0, 10000 :: Int)
    return x

-- =============================================================================
-- Exercise 3 :: Time spent: +- 20 minutes
-- Since every whole number over 1 is either a composite number or a prime number
-- I can check if the number is not a prime number
-- =============================================================================
exercise3 = do
  print $ take 100 composites'

composites' :: [Integer]
composites' = filter (not.prime) [4..]
-- =============================================================================
-- Exercise 4 :: Time spent: +- 1 hour
-- The smallest composite number that passes the test is 9
-- If k = 1 it runs fast if k = 2 then takes a little longer but comes to the same conclusion. Running k = 3
-- takes a lot longer and got as low as 15 in one test.
-- =============================================================================
exercise4 = do
  k1 <- testFer (testFermatKn 1)
  k2 <- testFer (testFermatKn 2)
  k3 <- testFer (testFermatKn 3)
  print k1
  print k2
  print k3
  
testFer tk = do
  x <- replicateM  1 tk
  let sorted = sort x 
  return $ head sorted

testFermatKn n= foolFermat' n composites

foolFermat' :: Int -> [Integer] -> IO Integer
foolFermat' k (x:xs) = do
    z <- primeTestsF k x
    if z then
      return x
    else
      foolFermat' k xs


-- =============================================================================
-- Exercise 5 :: Time spent: +- 2 hours
-- This function uses J. Chernick's theorem to construct a subset of carmichael numbers.
-- The fermat test is easily by the first 2 numbers produced by the carmichael function
-- =============================================================================
exercise5 = do
  k1 <- testFer (testFermatCarmichaelKn 1)
  k2 <- testFer (testFermatCarmichaelKn 2)
  k3 <- testFer (testFermatCarmichaelKn 3)
  print k1
  print k2
  print k3

testFermatCarmichaelKn n= foolFermat' n carmichael

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
          k <- [2..], 
          prime (6*k+1), 
          prime (12*k+1), 
          prime (18*k+1) ]

-- =============================================================================
-- Exercise 6 (1) :: Time spent: +-
-- =============================================================================
exercise6 = do
  print()

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
