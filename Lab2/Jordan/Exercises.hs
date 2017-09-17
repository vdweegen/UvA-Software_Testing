module Lab2 where

import Data.List
import Data.Char
import Data.Maybe
import System.Random
import Test.QuickCheck
import Lab2.Util.Random
import Lab2.Util.Ibans

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
-- Willem suggested the Chi Squared test to check for uniform distribution. 
-- This test uses your expected values and observed values and gives you a number this number must be lower then a specific critical value for
-- a test to be accepted.

quantiles :: [Float] -> [Float] -> [Integer]
quantiles xs [] = []
quantiles xs (q:qs) = [genericLength $ filter (<q) xs] ++ (quantiles (filter (>=q) xs) qs)

quantilesIO xs q = do {
    p <- probs xs;
    let r = quantiles p [x/q | x  <- [1..q]]
    in print $  (chi r) 
}


chi' n m x= div ((x - e) ^ 2) e
    where e = div n m

chi = sum.map(chi' 10000 4) 

exercise1 = quantilesIO 10000 4


-- Exercise 2
-- Estimated time taken 2 hours
-- Tried to think about the problem in an abstract way. Thus the most time was spent learning about triangles! XP
isTriangle' = (all ((\xs -> head xs <= sum (tail xs)) )) . permutations

isTriangle abc = sum ab <= c
            where 
            c = maximum abc
            ab = filter (<c) abc

isRightTriangle abc = c == sum ab
    where abc2 = map(^2) abc
          c = maximum abc2
          ab = filter (<c) abc2
          
equalSides = length.group.sort


triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z 
    | (not.isTriangle) abc =  NoTriangle
    | isRightTriangle abc = Rectangular
    | 2 == equalSides abc = Isosceles
    | 1 == equalSides abc = Equilateral
    | otherwise = Other
    where abc = [x,y,z]
    
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

exercise2 = do 
    print $ triangle 1 5 1 
    print $ triangle 3 5 3
    print $ triangle 4 4 4
    print $ triangle 1 1 5 
    print $ triangle 1 9 5 
    print $ triangle 2 5 5 
    print $ triangle 3 4 5  
    print $ triangle 1 2 3   

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
exercise4 = print $ isPermutation [3,2,1] [1,2,3]
-- Same length
-- Sorted same

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = null $ (\\) xs ys


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

prop_samelength xs = length xs == length (rot13 xs)
prop_notEqual xs = xs /= rot13 xs


exercise7 = do
        print $ (rot13d.rot13.rot13d) "GBB ZNAL FRPERGF!?"
        -- print $ rot13 ""
        quickCheck prop_samelength
        quickCheck prop_notEqual

-- Bonus Exercises


movetoback n xs = (drop n xs) ++ (take n xs)

--Utils
numbers xs = foldr (++) "" xs 
charcodes = (zip ['A'..'Z'] [10..35])
readStringInt x = (read x :: Integer)

translate = map (translateAlpha)       
translateAlpha :: Char -> String
translateAlpha n 
               | isAlpha n = (show.snd.(findInTuples charcodes)) n
               | otherwise = [n] 
               
findInTuples :: (Eq a) => [(a, b)] -> a -> (a, b)
findInTuples ts n  = head $ filter ((== n).fst) ts

isUpperAlphaNum :: Char -> Bool 
isUpperAlphaNum c = isUpper c || isDigit c

isValidCharacters :: String -> Bool
isValidCharacters = all isUpperAlphaNum 

transformIban = readStringInt.numbers.translate.movetoback 4 

isCharType :: (Char -> Bool) -> String -> Bool
isCharType y x = all y  x

sublist :: Int -> Int -> [a] -> [a]
sublist n m l = take m $ drop n l

hasCountryCode :: String -> Bool
hasCountryCode x = isCharType isAlpha $ (sublist 0 2 x)

hasCheckDigits :: String -> Bool
hasCheckDigits x =  isCharType isDigit $ (sublist 2 2 x)

iban :: String -> Bool
iban x =  34 > length x && isValidCharacters x &&  mod (transformIban x) 97 == 1

 --- One function :D Bosslike
ibanAlt x = 34 > length x && hasCountryCode x && hasCheckDigits x && all (\p ->  isUpper p || isDigit p ) x && mod (read (concatMap ibancalc $ drop 4 x ++ take 4 x) :: Integer) 97 == 1
                where
                ibancalc c | isAlpha c = show.(+10).fromJust $ elemIndex c ['A'..'Z'] 
                        | otherwise = [c]

exercise8 = do
    putStrLn "Modular method"
    print $ all iban validIbans
    putStrLn "CodeGolf method"
    print $ all ibanAlt validIbans               

exercisebonus = print ()
