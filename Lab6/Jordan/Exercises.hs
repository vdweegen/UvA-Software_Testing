module Lab6 where

import Control.Monad 
import Lecture6
import  Data.List

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
-- Exercise 1 :: Time spent: +-
-- =============================================================================

exercise1 = do
  print()

-- =============================================================================
-- Exercise 2 :: Time spent: +-
-- =============================================================================
exercise2 = do
  print()



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
  x <- replicateM  100 tk
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
-- Exercise 5 :: Time spent: +-
-- =============================================================================
exercise5 = do
  print()

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
