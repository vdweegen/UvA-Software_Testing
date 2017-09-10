module Exercises where

import Data.Bits

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- Exercise 4: Reusing reversal from exercise
reversal :: Integer -> Integer
reversal = read . reverse . show

-- helper method to check if reversal is also prime
primeReverse :: Integer -> Bool
primeReverse n = prime n && prime (reversal n)

-- simply filter the list
solution4a :: [Integer]
solution4a = filter primeReverse [0..9999]

-- bit more efficient, instead of first generating the entire list,
-- now only create a list with the correct values on the fly
-- Time spent: 5 min
solution4b :: [Integer]
solution4b = [a | a <- [1..9999], primeReverse a]

-- Exercise 5:
--

-- Exercise 6:
--
primeProduct :: Int -> Bool
primeProduct n = prime ((product $ take n primes) + 1)

solution6 :: Int
solution6 = head $ filter (primeProduct) [1..]


-- Exercise 7: see Luhn.sh

-- Exercise 8:
-- Encode everything as Haskell
-- Also provide base case when a boy doesn't mention all the boys otherwise we get a match error
-- We need three accusers to make someone guilty as 3 of them are speaking the truth
-- The boys who accused the guilty boy are honest
data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool

-- Matthew: Carl didn't do it, and neither did I.
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True

-- Peter It was Matthew or it was Jack.
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False

-- Jack Matthew and Peter are both lying.
accuses Jack b = not ( accuses Matthew b) && not ( accuses Peter b)

-- Arnold Matthew or Peter is speaking the truth, but not both.
accuses Arnold b =  accuses Matthew b `xor` accuses Peter b

-- Carl What Arnold says is not true.
accuses Carl b = not ( accuses Arnold b)

accusers :: Boy -> [Boy]
accusers b = filter (\x -> accuses x b) boys

guilty, honest :: [Boy]
guilty = filter (\x -> length (accusers x) == 3 ) boys
honest = accusers $ head guilty
