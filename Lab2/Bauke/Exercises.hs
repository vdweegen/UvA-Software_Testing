module Exercises where

import Data.Char
import Data.List

import Lab2.Util.Ibans
import Lab2.Util.Infix
import Lab2.Util.Primes
import Lab2.Util.Random

import Test.QuickCheck

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


-- Exercise 1 : Checking random function
-- Implementation time: 60 minutes
-- This took very long due to the IO stuff. We handled it in the summer school, but it took some time to figure out anyway.
-- So far, i can SEE that the distribution is correct, but the next step is to also SHOW that is is correct.

exercise1 =  probs 10000 >>= (\list -> return $ isEvenlyDistributed list)

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

-- Exercise 2
-- Implementation finished in 10 minutes, without the tests
-- Simply keying in the definitions for the triangles
-- The pythagorean algorithm can probably be refactored by sorting a b c and then taking 2 and comparing against last
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other
             deriving (Eq, Show)

exercise2 = do
              quickCheck prop_noTriangle
              quickCheck prop_equilateral
              quickCheck prop_isosceles
              quickCheck prop_rectangular
              quickCheck prop_other

prop_noTriangle (Positive a) (Positive b) (Positive c) = triangleCombinations a b (a+b+c) NoTriangle
prop_equilateral (Positive a) = triangleCombinations a a a Equilateral
prop_isosceles (Positive a) (Positive b) = triangleCombinations a a (a+b) Isosceles
prop_rectangular (Positive a) = validateRectangular $ pythagoreanTriplets !! a
prop_other (Positive a) (Positive b) (Positive c) = triangleCombinations a b c Other

pythagoreanTriplets :: [[Integer]]
pythagoreanTriplets = generateTriplets 250

generateTriplets :: Integer -> [[Integer]]
generateTriplets n = [ [a,b,c] | a <- [1..n], b <- [1..n], c <- [1..n], (a^2) + (b^2) == (c^2)]

validateRectangular :: [Integer] -> Bool
validateRectangular (a:b:c:[]) = triangleCombinations a b c Rectangular

triangleCombinations :: Integer -> Integer -> Integer -> Shape -> Bool
triangleCombinations a b c expectedType = allAre expectedType $ [triangle a b c, triangle a c b,
                                          triangle b a c, triangle b c a,
                                          triangle c a b, triangle c b a]

sampleTriangles = do
              print $ triangle 60 80 100 -- default Rectangular used in construction
              print $ triangle 10 10 10  -- Equilateral
              print $ triangle 1 1 100 -- Nothing
              print $ triangle 10 10 9 -- IsoSceles
              print $ triangle 10 9 8 -- Something else

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = evaluateShape $ sort [a,b,c]

evaluateShape :: [Integer] -> Shape
evaluateShape (a:b:c:_) | invalidTriangle a b c = NoTriangle
                        | (a == b) && (a == c) = Equilateral
                        | (a^2) + (b^2) == (c^2) = Rectangular
                        | (a == b ) || (a == c) || (b == c) = Isosceles
                        | otherwise = Other

invalidTriangle :: Integer -> Integer -> Integer -> Bool
invalidTriangle a b c = (a + b < c);

-- Exercise 3a
-- Implementation time: 15 minutes
-- Writing the generic function took most time,
-- Afterwards i noticed that haskell provides `isSubSet`

-- Exercise 3b
-- Implementation time: 20 minutes
-- Checked on the usage custom sort algorithm. Haskell provides sortBy.
-- Implemented the Ordering operator for the lists and passed this to the sort
-- Note that this ordering assumes that all sets are contained in the powerset.
-- If neither set is stronger, the order will remain unchanged.

data PropertyStrength = Left_Stronger | Right_Stronger | Both_Stronger | Invalid_Comparison
  deriving (Eq, Show)

-- Implement the properties and show which one is stronger
exercise3a = do
              putStr "'(\\ x -> even x && x > 3)' compared to 'even': "
              print $ compareProperties prop3aList evenList
              putStr "'(\\ x -> even x || x > 3)' compared to 'even': "
              print $ compareProperties prop3bList evenList
              putStr "'(\\ x -> (even x && x > 3) || even x)' compared to 'even': "
              print $ compareProperties prop3cList evenList
              putStr "'even' compared to '(\\ x -> (even x && x > 3) || even x)': "
              print $ compareProperties evenList prop3cList

exercise3b = print $ sortBy compareSets [prop3aList, prop3bList, prop3cList, evenList]

prop3aList :: [Integer]
prop3aList = filter (>3) evenList

prop3bList :: [Integer]
prop3bList = sort $ asSet $ merge largerThan3 evenList

prop3cList :: [Integer]
prop3cList = sort $ asSet $ merge prop3aList evenList

largerThan3 :: [Integer]
largerThan3 = filter (>3) domain

evenList :: [Integer]
evenList = filter even domain

compareProperties :: [Integer] -> [Integer] -> PropertyStrength
compareProperties lhs rhs | isSubset lhs rhs && isSubset rhs lhs = Both_Stronger
                          | isSubset lhs rhs = Left_Stronger
                          | isSubset rhs lhs = Right_Stronger
                          | otherwise = Invalid_Comparison

compareSets :: [Integer] -> [Integer] -> Ordering
compareSets xs ys | isSubset xs ys = LT
                  | isSubset ys xs = GT
                  | otherwise = EQ

domain :: [Integer]
domain = [-10..10]

merge :: [Integer] -> [Integer] -> [Integer]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- Exercise 4
exercise4 = print $ isPermutation [1,2,3,4] [4,2,3,1]

-- implementation of perms using the earlier provided isSubset method
-- note, as specified in the assignment, it does not process lists with dupes dupes.
-- If the dupes were important, one could simply count occurrences and compare this.
-- if xs /= ys and sort xs == sort ys, then it's a permutation.
-- For the testing procedure, it means you have to remove any duplicate from the input lists

-- properties:
-- lists have the same length
-- lists contain the same items

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = (length xs) == (length ys) && isSubset xs ys && isSubset ys xs

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset [] set = True
isSubset (x:xs) set = elem x set && isSubset xs set

asSet :: Eq a => [a] -> [a]
asSet [] = []
asSet (x:xs) | elem x xs = asSet xs
             | otherwise = x : asSet xs

-- Exercise 5
-- Implementation time: 10 minutes for the logic
-- Simply reusing the code for checking a permutation and making it stronger
-- by checking for the unique indexes

exercise5 = print $ isDerangement [1,2,3,4,5] [2,3,4,5,1]

isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement xs ys | invalidPermutation xs ys = False
                    | otherwise = uniqueIndexes xs ys

uniqueIndexes :: [Integer] -> [Integer] -> Bool
uniqueIndexes [] [] = True
uniqueIndexes (x:xs) (y:ys) | x == y = False
                            | otherwise = uniqueIndexes xs ys

invalidPermutation :: [Integer] -> [Integer] -> Bool
invalidPermutation xs ys | xs == ys = True
                         | otherwise = sort xs /= sort ys
-- Exercise 6
-- property of ROT 13
-- implementation time: 30 minutes
-- implementation time for tests: 60 minutes.
-- Thinking on how to
-- it maintains the case of the character
-- when performed twice, it returns the same character
-- the output is always different from the input
-- non printable chars are not converted
-- Since there is a finite set of characters and the function is mapped Char -> Char
-- Simply generating a set of all printable characters would suffice.
-- However, for random data's sake, some length & case properties are tested using random Strings

exercise6 = do
              quickCheck prop_GeneratesSameOutputForSameInput
              quickCheck prop_ReversibleWhenAppliedTwice
              quickCheck prop_MaintainsCase
              quickCheck prop_MaintainsLength
              quickCheck prop_ChangesAllAlphaCharacters
              quickCheck prop_IgnoresAllNonAlphaCharacters

-- Requires generators
-- one could argue that using a random a .. z generator.
-- but even with infinite tests, 100% coverage is never guaranteed
-- simply mapping the conversion over all available chars will yield guaranteed coverage
prop_ChangesAllAlphaCharacters =
  map id alphaCharacters /= map rot13 alphaCharacters

-- Same for the nonAlpha chars. Simply mapping it to all characters known to be ignored
-- yields 100% coverage.
prop_IgnoresAllNonAlphaCharacters =
  map id nonAlphaCharacters == map rot13 nonAlphaCharacters

nonAlphaCharacters :: [Char]
nonAlphaCharacters = ([(chr 0x20) .. (chr 0x7E)] \\ alphaCharacters)

alphaCharacters :: [Char]
alphaCharacters = ['A'..'Z'] ++ ['a'..'z'] ;

prop_ReversibleWhenAppliedTwice :: String -> Bool
prop_ReversibleWhenAppliedTwice text =
  text == (maskString $ maskString text)

prop_MaintainsCase text =
 (map isLowerCase (maskString text) == map isLowerCase text)
 && (map isUpperCase (maskString text) == map isUpperCase text)

prop_MaintainsLength :: String -> Bool
prop_MaintainsLength text =
  length text == length (maskString text)

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
-- Implementation time: 45 minutes
-- Had some issues reading back the string to an integer
-- The algorithm was fairly simply, followed wiki steps and got it first time right

-- Specifications for the iban
-- country code = 2 chars
-- check number = 2 chars
-- rest is max 30 chars

-- randomize the checksum
--

exercise7 = quickCheck prop_checkValidExamples

prop_checkValidExamples = allAre True (map iban validIbans)

allAre :: Eq a => a -> [a] -> Bool
allAre _ [] = True
allAre a (x:xs) = a == x && allAre a xs

iban :: String -> Bool
iban account | not $ validAccount account = False
             | otherwise = calculatedNumber account == actualNumber account

validAccount :: String -> Bool
validAccount (a:b:c1:c2:xs) = isAlpha a && isAlpha b && isNumber c1 && isNumber c2 && (inLimits (genericLength xs) (0,30))

inLimits :: Integer -> (Integer, Integer) -> Bool
inLimits act (min,max) = min <= act && max >= act

calculatedNumber :: String -> Integer
calculatedNumber account = (-) 98 $ flip mod 97 $ read (convertChars $ preProcess account) :: Integer

actualNumber :: String -> Integer
actualNumber (_:_:c1:c2:_) = read ([c1] ++ [c2]) :: Integer

preProcess :: String -> String
preProcess (a:b:_:_:rest) = rest ++ [a] ++ [b]

convertChars :: String -> String
convertChars [] = "00"
convertChars (x:xs) | ('A' <= x) && ('Z' >= x) = (show $ (+) 10 $ (ord x) - (ord 'A')) ++ convertChars xs
                    | otherwise = [x] ++ convertChars xs

-- 83,59,21
-- 29, 51

-- Bonus Exercises
exercisebonus = do
                  print $ euler29 [2..100]

-- a ^ b => generates 15 distinct terms for a 2..5 and b 2..5
-- how many terms does a^b generate for a 2 .. 100 and b 2..00
euler29 :: [Integer] -> Int
euler29 domain = length $ asSet [ a^b | a <- domain, b <-domain ]
