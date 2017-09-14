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
-- Seems to be evenly distributed  Time taken: about 1.5 to 2 hours

quantiles :: [Float] -> [Float] -> [Integer]
quantiles xs [] = []
quantiles xs (q:qs) = [genericLength $ filter (<q) xs] ++ (quantiles (filter (>=q) xs) qs)

quantilesIO xs q = do {
    p <- probs xs;
    print $ quantiles p [x/q | x  <- [1..q]]
}

exercise1 = quantilesIO 10000 4

-- Exercise 2
-- Estimated time taken 2 hours
-- Tried to think about the problem in an abstract way. Thus the most time was spent learning about triangles! XP
isTriangle = (all ((\xs -> head xs <= sum (tail xs)) )) . permutations

isRightTriangle abc = c == sum ab
    where abc2 = map(^2) abc
          c = maximum abc2
          ab = filter (<c) abc2
          
equalSides = length.group.sort

triangle x y z 
    | (not.isTriangle) abc =  NoTriangle
    | isRightTriangle abc = Rectangular
    | 2 == equalSides abc = Isosceles
    | 1 == equalSides abc = Equilateral
    | otherwise = Other
    where abc = [x,y,z]
    
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

exercise2 = print $ triangle 4 5 5

-- Exercise 3a

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

isEvenGT3  :: Integer -> Bool 
isEvenGT3 x = even x && x > 3

isEvenOrGT3 :: Integer -> Bool 
isEvenOrGT3 x = even x || x > 3

isEvenGT3OrEven :: Integer -> Bool 
isEvenGT3OrEven x = isEvenGT3 x || even x

-- Keep it simple stupid
strength a b | stronger [-10..10] (snd a) (snd b) = LT
    | otherwise = GT

propertiesExercise3 = [("even", even),("isEvenGT3", isEvenGT3), ("isEvenOrGT3", isEvenOrGT3), ("isEvenGT3OrEven", isEvenGT3OrEven)]

exercise3a = print $ map fst $ propertiesExercise3

-- Exercise 3b
exercise3b = print $ map fst $ sortBy strength propertiesExercise3

-- Exercise 4
exercise4 = print()

-- Exercise 5
exercise5 = print()

-- Exercise 6
exercise6 = print()

-- Exercise 7

letters x | isLower x = ['a'..'z'] 
    | otherwise =  ['A'..'Z']

rot t direction x | isAlpha x = tletter
   |otherwise = x
   where 
         searchSpace = zip (letters x) [0..25]
         tindex  =  direction t $ (snd.head.filter((==x).fst)) searchSpace
         tletter =  fst.head $ filter((==(mod tindex 26)).snd) searchSpace
                    
rot13  = map (rot 13 (+)) 
rot13d = map  (rot 13 subtract) 

cc  = map (rot 23 (+)) 
ccd = map  (rot 23 subtract) 


exercise7 = print  $ (rot13d.rot13.rot13d) "GBB ZNAL FRPERGF"

-- Bonus Exercises
exercisebonus = print()
