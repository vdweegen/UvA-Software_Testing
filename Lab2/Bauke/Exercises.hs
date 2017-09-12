<<<<<<< HEAD
module Exercises where
import Data.List

import Lab2.Util.Random

main = do
        putStr "Distribution of 10000 random values: "
        exercise1

exercise1 = probs 10000 >>= (\list -> return $ isEvenlyDistributed list)

isEvenlyDistributed :: [Float] -> (Integer, Integer, Integer, Integer)
isEvenlyDistributed = sumTuples . categorize

categorize :: [Float] -> [(Integer, Integer, Integer, Integer)]
categorize fs = [ b | a <- fs, let b = createTuple a]

createTuple :: Float -> (Integer, Integer, Integer, Integer)
createTuple float | float < 0.25 = (1,0,0,0)
                  | float < 0.50 = (0,1,0,0)
                  | float < 0.75 = (0,0,1,0)
                  | otherwise = (0,0,0,1)

sumTuples :: [(Integer, Integer, Integer, Integer)] -> (Integer,Integer,Integer,Integer)
sumTuples xs = doSum xs (0,0,0,0)

doSum :: [(Integer, Integer, Integer, Integer)] -> (Integer,Integer,Integer,Integer) -> (Integer,Integer,Integer,Integer)
doSum [] a = a
doSum ((a1,a2,a3,a4):xs) (b1,b2,b3,b4) = doSum xs (a1+b1, a2+b2, a3+b3, a4+b4)




=======
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Assignment 2 / Lab 2 :: Group 14 --

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Define Main --
main = do
    putStrLn $ "===================="
    putStrLn $ "Assignment 2 / Lab 2"
    putStrLn $ "===================="
    putStrLn $ "> Exercise 1"
    exercise1
    putStrLn $ "> Exercise 2"
    exercise2
    putStrLn $ "> Exercise 3a"
    exercise3a
    putStrLn $ "> Exercise 3b"
    exercise3b
    putStrLn $ "> Exercise 4"
    exercise4
    putStrLn $ "> Exercise 5"
    exercise5
    putStrLn $ "> Exercise 6"
    exercise6
    putStrLn $ "> Exercise 7"
    exercise7
    putStrLn $ "> BONUS"
    exercisebonus

-- Exercise 1
exercise1 = print()

-- Exercise 2
exercise2 = print()

-- Exercise 3a
exercise3a = print()

-- Exercise 3b
exercise3b = print()

-- Exercise 4
exercise4 = print()

-- Exercise 5
exercise5 = print()

-- Exercise 6
exercise6 = print()

-- Exercise 7
exercise7 = print()

-- Bonus Exercises
exercisebonus = print()
>>>>>>> cc7741c86c8b500a3d9b3bf9dcf8b3a638ba7fe2
