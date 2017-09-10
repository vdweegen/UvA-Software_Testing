module Lab1 where
import Data.List
import Test.QuickCheck
import Data.Bits


-- Assignment 1 / Lab 1 :: Group 14 --

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Define Main --
main = do
    putStrLn $ "===================="
    putStrLn $ "Assignment 1 / Lab 1"
    putStrLn $ "===================="
    putStrLn $ "> Exercise 1.1"
    exercise1_1
    putStrLn $ "> Exercise 1.2"
    exercise1_2
    putStrLn $ "> Exercise 2"
    exercise2
    putStrLn $ "> Exercise 3"
    exercise3
    putStrLn $ "> Exercise 4"
    exercise4
    putStrLn $ "> Exercise 5"
    exercise5
    putStrLn $ "> Exercise 6"
    exercise6
    putStrLn $ "> Exercise 7"
    exercise7
    putStrLn $ "> Exercise 8"
    exercise8
    putStrLn $ "> BONUS"
    exercisebonus

-- Exercise 1.1
basecase1 :: Integer -> Integer
inductioncase1 :: Integer -> Integer
basecase1 n = sum [ a^2 | a <- [0..n]]
inductioncase1 n = (n*(n+1)*(2*n+1)) `div` 6
exercise1_1 = quickCheckResult(\n -> n >= 0 --> basecase1 n == inductioncase1 n)

-- Exercise 1.2
basecase2 :: Integer -> Integer
inductioncase2 :: Integer -> Integer
basecase2 n = sum [ a^3 | a <- [0..n]]
inductioncase2 n = ((n*(n+1)) `div` 2 ) ^ 2
exercise1_2 = quickCheckResult(\n -> n >= 0 --> basecase2 n == inductioncase2 n)

-- Exercise 2
prop_subsequenceSize :: [Integer] -> Bool
prop_subsequenceSize n =
  (^) 2 (genericLength n) == genericLength (subsequences n)
exercise2 = quickCheckWith stdArgs { maxSize = 20 } prop_subsequenceSize

-- Exercise 3
factorial n  = product [1..n]
solution3 (Positive n) = (length $ permutations [1..n]) == factorial(n)
exercise3 = quickCheckWith stdArgs { maxSize = 10 } solution3

-- Exercise 4
reversal :: Integer -> Integer
reversal = read . reverse . show

-- helper method to check if reversal is also prime
primeReverse :: Integer -> Bool
primeReverse n = prime n && prime (reversal n)

-- simply filter the list
-- solution4a :: [Integer]
-- solution4a = filter primeReverse [0..9999]

-- bit more efficient, instead of first generating the entire list,
-- now only create a list with the correct values on the fly
solution4b :: [Integer]
solution4b = [a | a <- [1..9999], primeReverse a]

exercise4 = do
  print $ solution4b

-- Exercise 5

-- It's way too slow. it will find for 2, but certainly not for 101.
-- 5 primes takes ages
-- main issue is probably the drop & take, it re-generates the list one every iteration

-- While the iteration is performed forwards, testing it would result in performing the same operation again.
-- All combinations could be checked, but this is exactly what the algorithm does.
-- If the result matches the new prime, it is valid

-- Whilst trying to optimize I noticed i forgot the termination in the elem call. So any non-prime number (4) would cause
-- the elem .. primes to run forever.
-- Fixed this by introducing primesTill ..., which returns the list up and including the argument passed

-- How to test is your answer is correct:
-- Take the list of all primes up to the maximum prime value in the solution list
-- Compose all lists of 101 primes from this list
-- Validate that NO sum of these lists is a prime

primesSum x y = sum $ take x $ drop y primes
primesconsum =  head $ filter (prime) $ map (primesSum 101) $ [0..]

exercise5 = do
    print $ primesconsum

-- Exercise 6
listOfCounters :: [[Integer]]
listOfCounters = [ take a primes | a <- [1..], let xs = take a primes, not $ prime $ 1 + product xs ]

exercise6 = do
  print $ head listOfCounters

-- Exercise 7
digits :: Integral x  => x -> [x]
digits 0 = []
digits x = digits (x `div` 10 ) ++ [x `mod` 10]

numbers :: [Integer] -> Integer
numbers [] = 0
numbers (x:xs) = x * (10 ^ (length xs)) + (numbers xs)

first :: [a] -> [a]
first [] = []
first (x:xs) = x:second xs

second :: [a] -> [a]
second [] = []
second (x:xs) = first xs

reverseDigits :: Integer -> [Integer]
reverseDigits =  reverse . digits

doubleDigits :: [Integer] -> [Integer]
doubleDigits = map (sum . digits)

sumDoubleDigits :: [Integer] -> Integer
sumDoubleDigits = sum . doubleDigits

luhnvalue :: Integer -> Integer
luhnvalue x = ((sum $ first $ reverseDigits x) +  (sumDoubleDigits $ map (*2) $ second $ reverseDigits x) )

luhn :: Integer -> Bool
luhn x =  (luhnvalue x )`mod` 10  == 0

checkPrefix :: Integer -> [Integer] -> Bool
checkPrefix x y = and (zipWith (==) y (digits x))

checkPrefixRange :: Integer -> [Integer]  -> Bool
checkPrefixRange x range = or $ map(checkPrefix x) $ map(digits) range

checkPrefixRanges :: Integer -> [[Integer]]  -> Bool
checkPrefixRanges x ranges  = or $ map(checkPrefixRange x) ranges

checkCardFormat :: [[Integer]] -> [Integer] -> Integer -> Bool
checkCardFormat prefixRanges numberLength x = checkPrefixRanges x prefixRanges && elem (genericLength $ digits x) numberLength

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress  = checkCardFormat [[34,37]] [15]
isMaster  = checkCardFormat [[51..55], [2221..2720]] [16]
isVisa  = checkCardFormat [[4]] [13, 16, 19]

mastercards = [
  5204740009900014,
  5420923878724339,
  5455330760000018,
  5506900490000436,
  5506900490000444,
  5506900510000234,
  5506920809243667,
  5506922400634930,
  5506927427317625,
  5553042241984105,
  5555553753048194,
  5555555555554444
  ]

visas = [
  4012888888881881,
  4111111111111111,
  4444333322221111,
  4911830000000,
  4917610000000000,
  4462030000000000,
  4917610000000000003
  ]

americanExpress = [
  371449635398431,
  378282246310005
  ]

exercise7 = do
    putStrLn "Mastercard cards"
    print $ all isMaster mastercards

    putStrLn "AmericanExpress cards"
    print $ all isAmericanExpress americanExpress

    putStrLn "Visa cards"
    print $ all isVisa visas

-- Exercise 8
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
accusers x = [y | y <- boys, accuses y x]

guilty, honest :: [Boy]
guilty = [x | x <- boys, length (accusers x) == 3]
honest = accusers $ (guilty !! 0)

exercise8 = do
  print "Guilty"
  print guilty
  print "Honest"
  print honest

-- Bonus Exercises

-- Euler 9
euler9 :: Integer
euler9 = head [a * b * c | a <- [1..1000], b <- [a..1000], let c = 1000 - a -b, a^2 + b^2 == c^2]

-- Euler 10
euler10 :: Integer
euler10 = sum $ takeWhile (< 2000000) primes

-- Euler 49
primes1000 :: [Integer]
primes1000 = dropWhile (< 1000) $ takeWhile (< 10000) primes

isPermutation :: (Integer, Integer, Integer) -> Bool
isPermutation (a, b, c) = elem (digits b) (permutations (digits a)) && elem (digits c) (permutations (digits a))

result :: (Integer, Integer, Integer) -> String
result (a, b, c) = (show a) ++ (show b) ++ (show c)

euler49 :: String
euler49 = result $ last $ filter isPermutation [(a , b, c) | a <- primes1000, let b = a + 3330, let c = b + 3330, b `elem` primes1000, c `elem` primes1000]

exercisebonus = do
  putStrLn "Project Euler #9"
  print euler9
  putStrLn "Project Euler #10"
  print euler10
  putStrLn "Project Euler #49"
  print euler49
