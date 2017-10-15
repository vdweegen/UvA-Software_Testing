module Lab6 where

import Lecture6
import Test.QuickCheck
import Data.Bits

import System.Clock
import System.Random

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
  putStrLn $ "Checking example. 3^200 mod 50: " ++ (show $ exM 3 200 50)
  putStrLn $ "Compare expM and exM. result equal: " ++ (show $ (exM 3 200 50) == (expM 3 200 50))
  putStrLn $ "Generating arbitrary amount, using quickCheck"
  quickCheck prop_exm

-- | Copied implementation from exM in the lecture.hs
exM' :: Integer -> Integer -> Integer -> Integer
exM' b 1 m = b `mod` m
exM' b e m | even e = squaredMod 1
           | odd e = squaredMod b
            where squaredMod v = v * (exM' b (e `shiftR` 1) m) ^ 2 `mod` m

prop_exm :: (Positive Integer, Positive Integer, Positive Integer) -> Bool
prop_exm (Positive b, Positive e, Positive m) = exM' b e m == expM b e m

-- =============================================================================
-- Exercise 2 :: Time spent: +- 3 hours
--
-- Initial attempt:
-- Original code 100000 primes: TimeSpec {sec = 48, nsec = 252859097}
-- Improved code 100000 primes: TimeSpec {sec = 44, nsec = 955916568}
--
-- 2nd attempt:
--
-- Finally figured out how to compile with profiling enabled:
-- ghc -O2 -rtsopts -prof --make Exercises.hs -main-is Lab6.main -o Exercises
--
-- and then execute: ./Exercises +RTS -p
--
-- This yields the following output:
-- =============================================================================
-- 	Tue Oct 10 21:11 2017 Time and Allocation Profiling Report  (Final)
--
-- 	   Exercises +RTS -p -RTS
--
-- 	total time  =        2.40 secs   (2397 ticks @ 1000 us, 1 processor)
-- 	total alloc = 1,989,224,136 bytes  (excludes profiling overheads)
--
-- COST CENTRE  MODULE        SRC                                %time %alloc
--
-- CAF          Lecture6      <entire-module>                     97.0   96.2
-- getStdRandom System.Random System/Random.hs:(586,1)-(587,26)    2.4    2.8
--
--
--                                                                                          individual      inherited
-- COST CENTRE   MODULE                SRC                               no.     entries  %time %alloc   %time %alloc
--
-- MAIN          MAIN                  <built-in>                         58          0    0.3    0.8   100.0  100.0
--  getStdRandom System.Random         System/Random.hs:(586,1)-(587,26) 117     100000    2.4    2.8     2.4    2.8
--  diffTimeSpec System.Clock          System/Clock.hsc:283:1-38         118          1    0.0    0.0     0.0    0.0
--  CAF          Lab6                  <entire-module>                   115          0    0.3    0.3     0.3    0.3
--   getTime     System.Clock          System/Clock.hsc:185:1-64         116          1    0.0    0.0     0.0    0.0
--  CAF          Lecture6              <entire-module>                   114          0   97.0   96.2    97.0   96.2
--  CAF          System.Random         <entire-module>                   113          0    0.0    0.0     0.0    0.0
--  CAF          Data.Time.Clock.POSIX <entire-module>                   111          0    0.0    0.0     0.0    0.0
--  CAF          System.Clock          <entire-module>                   110          0    0.0    0.0     0.0    0.0
--  CAF          GHC.Conc.Signal       <entire-module>                    99          0    0.0    0.0     0.0    0.0
--  CAF          GHC.IO.Encoding       <entire-module>                    97          0    0.0    0.0     0.0    0.0
--  CAF          GHC.IO.Handle.FD      <entire-module>                    96          0    0.0    0.0     0.0    0.0
--  CAF          GHC.IO.Handle.Text    <entire-module>                    95          0    0.0    0.0     0.0    0.0
--  CAF          GHC.IO.Encoding.Iconv <entire-module>                    85          0    0.0    0.0     0.0    0.0
--
--
-- Profiling results gave me a line that I needed to fix, which resulted into the
-- following results:
-- Improved code 100000 primes: TimeSpec {sec = 2, nsec = 651626570}
-- Note: In order to correctly profile the function, execute :set +r on the commandline, to disable caching
-- =============================================================================
exercise2 = do
  let n = 100000
  testOriginal <- testTime $ mapM primeTestF (take n primes)
  print $ "Testing " ++ (show n) ++ " primes with original code"
  print $ testOriginal
  testRefactored <- testTime $ mapM primeTest (take n primes)
  print $ "Testing " ++ (show n) ++ " primes with improved code"
  print $ testRefactored

testTime :: IO a -> IO (TimeSpec)
testTime f = do
  start <- getTime Monotonic
  f
  end <- getTime Monotonic
  return (diffTimeSpec start end)

-- Modified version from Lecture6.hs
primeTest :: Integer -> IO Bool
primeTest n = do
   a <- randomRIO (2, n-1) :: IO Integer
   return $ (exM a (n-1) n) == 1

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
