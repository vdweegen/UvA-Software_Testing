module Lab2.Exercises where

import Data.List
import Data.Maybe
import Test.QuickCheck
import Data.Bits
import System.Random
import Control.Monad
import Control.Applicative
import Data.Char
import Lab2.Util.Ibans

-- Assignment 2 / Lab 2 :: Group 14 --

-- Define Main --
main = do
    putStrLn "===================="
    putStrLn "Assignment 2 / Lab 2"
    putStrLn "===================="
    putStrLn "> Exercise 1"
    exercise1
    putStrLn "> Exercise 2"
    exercise2
    putStrLn "> Exercise 3a"
    exercise3a
    putStrLn "> Exercise 3b"
    exercise3b
    putStrLn "> Exercise 4"
    exercise4
    putStrLn "> Exercise 5"
    exercise5
    putStrLn "> Exercise 6"
    exercise6
    putStrLn "> Exercise 7"
    exercise7
    putStrLn "> BONUS"
    exercisebonus

-- provided
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- Exercise 1
-- About one hour also to think of chi test
quartile :: Float -> Float -> Float -> Bool
quartile min max n = n >= min && n < max

quartile1, quartile2, quartile3, quartile4 :: Float -> Bool
quartile1 = quartile 0 0.25
quartile2 = quartile 0.25 0.5
quartile3 = quartile 0.5 0.75
quartile4 = quartile 0.75 1

chi :: Int -> Int -> Float
chi x m = fromIntegral((x-m)^2) / fromIntegral m


distribution :: Int -> IO ()
distribution n = do
                    p <- probs n
                    let a = length $ filter quartile1 p
                    let b = length $ filter quartile2 p
                    let c = length $ filter quartile3 p
                    let d = length $ filter quartile4 p
                    let m = div n 4
                    let x = chi a m + chi b m + chi c m + chi d m
                    print (a,b,c,d,x)

exercise1 = distribution 10000

-- Exercise 2
-- Encoded properties for each triangle
-- 15 min
exercise2 = print()
data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | noTriangle a b c = NoTriangle
               | equilateral a b c = Equilateral
               | isosceles a b c = Isosceles
               | rectangular a b c = Rectangular
               | otherwise = Other

isTriangle, noTriangle, equilateral, isosceles, rectangular :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = (a + b > c) || (a + c > b) || (b + c > a)
noTriangle a b c = not $ isTriangle a b c
equilateral a b c = (a == b) && (a == c) && (b == c)
isosceles a b c = (a == b) || (a == c) || (b == c)
rectangular a b c = (a^2+b^2) == c^2 || (a^2+c^2) == b^2 || (b^2+c^2) == a^2

equilateralProp, isoscelesProp, rectangularProp :: Int -> [(Int, Int, Int)]
equilateralProp n = [(a,b,c)| a <- [1..n], b <- [1..n], c <- [1..n], a == b && b == c]
isoscelesProp n   = [(a,b,c)| a <- [1..n], b <- [1..n], c <- [1..n], (a == b) && (a == c) && (b == c)]
rectangularProp n = [(a,b,c)| a <- [1..n], b <- [1..n], c <- [1..n], (a^2+b^2) == c^2 || (a^2+c^2) == b^2 || (b^2+c^2) == a^2]

-- Exercise 3a
exercise3a = print()

-- Exercise 3b
exercise3b = print()

-- Exercise 4
exercise4 = print()
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = False
isPermutation xs ys = (length xs == length ys)
                      && forall xs (\x-> x `elem` ys)
                      && forall ys (\y-> y `elem` xs)

-- Exercise 5
-- 10 min
-- reuse isPermutation, we know the elements are there so we can call fromJust on elemIndex
-- simply compare the index of the relevant element with the other list
-- use permutations to generate a permutation, only add it to the list if it satisfies isDerangement
exercise5 = print()
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys
                      && forall xs (\x -> (fromJust $ elemIndex x xs) /= (fromJust $ elemIndex x ys))

deran :: Eq a => [a] -> [[a]]
deran xs = [ys | ys <- permutations xs, isDerangement xs ys]

-- Exercise 6
-- simply fetch index in the array and get letter from the other array
-- 15 min
exercise6 = quickCheckResult rotSpec
upper, lower, upperRot13, lowerRot13 :: String
upper = ['A'..'Z']
lower = ['a'..'z']
upperRot13 = ['N'..'Z'] ++ ['A'..'M']
lowerRot13 = ['n'..'z'] ++ ['a'..'m']

rot13 :: Char -> Char
rot13 c | c `elem` upper = upperRot13 !! fromJust(elemIndex c upper)
        | c `elem` lower = lowerRot13 !! fromJust(elemIndex c lower)
        | otherwise = c

rot13string :: String -> String
rot13string = map rot13

rotSpecLength :: String -> Bool
rotSpecLength s = length(rot13string s) == length s

rotSpecNotEqual :: String -> Bool
rotSpecNotEqual [] = True
rotSpecNotEqual s = rot13string s /= s

rotSpecEqual :: String -> Bool
rotSpecEqual s = rot13string(rot13string s) == s

rotSpec :: String -> Bool
rotSpec s | null(strip s) = True
          | otherwise = rotSpecLength s && rotSpecNotEqual s && rotSpecEqual s

strip :: String -> String
strip = filter(\x -> x `elem` (['a'..'z'] ++ ['A'..'Z']))

-- Exercise 7
iban :: String -> Bool
iban s = mod (toNumbers $ first4 s) 97 == 1

first4 :: String -> String
first4 (a:b:c:d:s) = s ++ [a,b,c,d]

toNum :: Char -> String
toNum c | c `elem` ['0'..'9'] = [c]
        | c `elem` ['A'..'Z'] = show $ maybe 0 (+10) (elemIndex c ['A'..'Z'])
        | otherwise = []


toNumbers :: String -> Integer
toNumbers s = read(concat $ map toNum s)::Integer

exercise7 = print(forall validIbans iban)

-- Bonus Exercises
exercisebonus = print()
