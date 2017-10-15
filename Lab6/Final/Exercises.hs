module Lab6 where

import Lecture6

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
  putStrLn $ "checking example. 3^200 mod 50: " ++ (show $ exM 3 200 50)
  putStrLn $ "compare expM and exM. result equal: " ++ (show $ (exM 3 200 50) == (expM 3 200 50))


-- | Copied implementation from exM in the lecture.hs
exM' :: Integer -> Integer -> Integer -> Integer
exM' b 1 m = b `mod` m
exM' b e m | even e = squaredMod 1
           | odd e = squaredMod b
            where squaredMod v = v * (exM' b (e `shiftR` 1) m) ^ 2 `mod` m

-- =============================================================================
-- Exercise 2 :: Time spent: +-
-- =============================================================================
exercise2 = do
  print()

-- =============================================================================
-- Exercise 3 :: Time spent: +-
-- =============================================================================
exercise3 = do
  print()

-- =============================================================================
-- Exercise 4 :: Time spent: +-
-- =============================================================================
exercise4 = do
  print()

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
