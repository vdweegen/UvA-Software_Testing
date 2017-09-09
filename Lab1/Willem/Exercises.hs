module Exercises where

-- Reusing reversal from exercise
reversal :: Integer -> Integer
reversal = read . reverse . show

-- helper method to check if reversal is also prime
primeReverse :: Integer -> Bool
primeReverse n = prime n && prime (reversal n)

-- simply filter the list
solution5a :: [Integer]
solution5a = filter primeReverse [0..9999]

-- bit more efficient, instead of first generating the entire list,
-- now only create a list with the correct values on the fly
-- Time spent: 5 min
solution5b :: [Integer]
solution5b = [a | a <- [1..9999], primeReverse a]

