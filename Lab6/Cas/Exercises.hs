module Lab6 where

import Lecture6

import System.Random (randomRIO)
import System.Clock

-- Define Main --
main = do
    putStrLn "===================="
    putStrLn "Assignment 6 / Lab 6"
    putStrLn "===================="
    putStrLn "> Exercise 1"
    -- exercise1
    putStrLn "> Exercise 2"
    exercise2
    putStrLn "> Exercise 3"
    -- exercise3
    putStrLn "> Exercise 4"
    -- exercise4
    putStrLn "> Exercise 5"
    -- exercise5
    putStrLn "> Exercise 6 (1)"
    -- exercise6
    putStrLn "> Exercise 6 (2)"
    -- exercise62
    putStrLn "> Exercise 7 (BONUS)"
    -- exercise7

-- =============================================================================
-- Exercise 1 :: Time spent: +-
-- =============================================================================
exercise1 = do
  print $ Lab6.exM 2 32 5

-- x is the base, y is the exponent, n is N, t is the term
exM :: Integer -> Integer -> Integer -> Integer
exM x 0 n = 1
exM x y n = t * Lab6.exM ((x * x) `mod` n) (y `div` 2) n `mod` n
  where t = if (not (y `mod` 2 == 0)) then x `mod` n else 1

-- =============================================================================
-- Exercise 2 :: Time spent: +-
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
-- =============================================================================
exercise2 = do
  -- testOriginal <- testTime $ mapM primeTestF (take 100000 primes)
  -- print $ "Testing 100000 primes with original code"
  -- print $ testOriginal
  testRefactored <- testTime $ mapM Lab6.primeTest (take 100000 primes)
  print $ "Testing 100000 primes with improved code"
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
   return (Lab6.exM a (n-1) n == 1)

-- =============================================================================
-- Exercise 3 :: Time spent: +-
-- =============================================================================
exercise3 = do
  print $ Lab6.composites

composites :: [Integer]
composites = 2 : filter (not . prime) [3..]

-- =============================================================================
-- Exercise 4 :: Time spent: +-
--
-- When the k is increased, the accuracy decreased (the number of hits on
-- primes decrease)
-- =============================================================================
exercise4 = do
  mapM isFooled (take 100 Lab6.composites)

-- Helper function to print
isFooled :: Integer -> IO ()
isFooled n = do
  x <- primeTests 1 n -- 1 is k
  if (x) then print n else return ()

-- Modified version from Lecture6.hs
primeTests :: Int -> Integer -> IO Bool
primeTests k n = do
 as <- sequence $ fmap (\_-> randomRIO (2,n-1)) [1..k]
 return (all (\ a -> Lab6.exM a (n-1) n == 1) as)

-- =============================================================================
-- Exercise 5 :: Time spent: +-
--
-- Feeding the Fermat's checker 500 carmichael number, it was only able to
-- detect 1 (out of 500) number (on average) as not being a prime.
--
-- That's an accuracy of around 0.2 %

-- Fiddling around with the value for K does not change the results
-- =============================================================================
exercise5 = do
  mapM isNotFooled (take 500 carmichael)

isNotFooled :: Integer -> IO ()
isNotFooled n = do
  x <- primeTests 4 n -- 1 is k
  if (x) then return () else print n

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
  k <- [2..],
  prime (6*k+1),
  prime (12*k+1),
  prime (18*k+1) ]

-- =============================================================================
-- Exercise 6 (1) :: Time spent: +-
--
-- Accuracy: 459 out of 500 detected.
-- =============================================================================
exercise6 = do
  mapM isFooledMR (take 500 carmichael)

isFooledMR :: Integer -> IO ()
isFooledMR n = do
  x <- Lab6.primeMR 1 n -- 1 is k
  if (x) then print n else return ()

isNotFooledMR :: Integer -> IO ()
isNotFooledMR n = do
  x <- Lab6.primeMR 1 n -- 1 is k
  if (x) then return () else print n

-- Modified version from Lecture6.hs
mrComposite :: Integer -> Integer -> Bool
mrComposite x n = let
    (r,s) = decomp (n-1)
    fs     = takeWhile (/= 1)
       (map (\ j -> Lab6.exM x (2^j*s) n)  [0..r])
  in
    Lab6.exM x s n /= 1 && last fs /= (n-1)

-- Modified version from Lecture6.hs
primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = do
  a <- randomRIO (2, n-1) :: IO Integer
  if Lab6.exM a (n-1) n /= 1 || Lab6.mrComposite a n
    then return False else Lab6.primeMR (k-1) n

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
