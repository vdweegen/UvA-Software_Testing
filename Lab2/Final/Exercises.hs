module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

import Lab2.Util.Random

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
-- QuickCheck for generating a number of random values and checking that the value is maintained
exercise1 = quantilesIO 10000 4

quantilesIO :: Int -> Int -> IO()
quantilesIO xs q = do {
    p <- probs xs;
    print $ flip chi (div xs q) $ quantiles p [ fromIntegral x / (fromIntegral q) | x  <- [1..q]]
}

chi :: [Int] -> Int -> Float
chi [] m = 0
chi (x:xs) m = (fromIntegral((x-m)^2) / fromIntegral m) + chi xs m

quantiles :: [Float] -> [Float] -> [Int]
quantiles xs [] = []
quantiles xs (q:qs) = [genericLength $ filter (<q) xs] ++ (quantiles (filter (>=q) xs) qs)


-- Exercise 2


data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

exercise2 = do
              quickCheck prop_Equilateral
              quickCheck prop_Isosceles
              quickCheck prop_Invalid

prop_Equilateral (Positive n) =
  Equilateral == triangle n n n

prop_Isosceles (Positive n) =
  True == ((Isosceles == (triangle n n (randBetween 1 (2*n))))
          && (Isosceles == (triangle n (randBetween 1 (2*n)) n))
          && (Isosceles == (triangle (randBetween 1 (2*n)) n n)))

prop_Invalid (Positive n) (Positive o) =
  True == (NoTriangle == triangle n o (randAbove (n+o)))

-- Not random, but just to fix quickCheck
randBetween :: Integer -> Integer -> Integer
randBetween a b = a + (div b 2)

randAbove :: Integer -> Integer
randAbove n = n + 10;

checkShape :: (Integer, Integer, Integer) -> Shape
checkShape (a,b,c) = triangle a b c

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

equilateralProp, isoscelesProp, rectangularProp :: Integer -> [(Integer, Integer, Integer)]
equilateralProp n = [(a,b,c)| a <- [1..n], b <- [1..n], c <- [1..n], a == b && b == c]
isoscelesProp n   = [(a,b,c)| a <- [1..n], b <- [1..n], c <- [1..n], (a == b) || (a == c) || (b == c)]
rectangularProp n = [(a,b,c)| a <- [1..n], b <- [1..n], c <- [1..n], (a^2+b^2) == c^2 || (a^2+c^2) == b^2 || (b^2+c^2) == a^2]

-- Exercise 3a
exercise3a = print()

-- Exercise 3b
exercise3b = print()

-- Exercise 4
exercise4 = print()

-- Exercise 5
exercise5 = print()

-- Exercise 6
exercise6 = do
              quickCheck prop_GeneratesSameOutputForSameInput
              quickCheck prop_ReversibleWhenAppliedTwice
              quickCheck prop_MaintainsCase
              quickCheck prop_MaintainsLength


-- Requires generators
prop_ChangesAllNormalCharacters = undefined

prop_ReversibleWhenAppliedTwice :: String -> Bool
prop_ReversibleWhenAppliedTwice text =
  text == (maskString $ maskString text)

prop_MaintainsCase text =
 (map isLowerCase (maskString text) == map isLowerCase text)
 && (map isUpperCase (maskString text) == map isUpperCase text)

prop_MaintainsLength :: String -> Bool
prop_MaintainsLength text =
  length text == length (maskString text)


prop_MaintainsOtherCharacters = undefined

prop_GeneratesSameOutputForSameInput :: String -> Bool
prop_GeneratesSameOutputForSameInput text =
  maskString text == maskString text

maskString :: String -> String
maskString input = [ rot13 a | a <- input]

rot13 :: Char -> Char
rot13 char | (isLowerCase char) && (char <= lowLowerHalf ) = chr $ (ord char) + 13
           | (isLowerCase char) && (char > lowLowerHalf) = chr $ (ord char) - 13
           | (isUpperCase char) && (char <= lowUpperHalf ) = chr $ (ord char) + 13
           | (isUpperCase char) && (char > lowUpperHalf) = chr $ (ord char) - 13
           | otherwise = char

lowLowerHalf :: Char
lowLowerHalf = chr $ (ord lowUpperHalf) + 0x20

lowUpperHalf :: Char
lowUpperHalf = chr $ (+) lowVal $ flip div 2 $ (ord 'Z') - lowVal
               where lowVal = ord 'A'

isLowerCase, isUpperCase :: Char -> Bool
isLowerCase char = ('a' <= char) && ('z' >= char)
isUpperCase char = ('A' <= char) && ('Z' >= char)

-- Exercise 7
exercise7 = print()

-- Bonus Exercises
exercisebonus = print()
