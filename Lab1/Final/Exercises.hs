module Lab1 where
import Data.List
import Test.QuickCheck
import Data.Bits


-- Assignment 1 / Lab 1 :: Group 14 --

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
    exerciseBonus

-- Exercise 1.1
exercise1_1 = quickCheckResult(\n -> n >= 0 --> baseCase1 n == inductionCase1 n)

baseCase1 :: Integer -> Integer
baseCase1 n = sum [ a^2 | a <- [0..n]]

inductionCase1 :: Integer -> Integer
inductionCase1 n = (n*(n+1)*(2*n+1)) `div` 6

-- Exercise 1.2
exercise1_2 = quickCheckResult(\n -> n >= 0 --> baseCase2 n == inductionCase2 n)

baseCase2 :: Integer -> Integer
baseCase2 n = sum [ a^3 | a <- [0..n]]

inductionCase2 :: Integer -> Integer
inductionCase2 n = ((n*(n+1)) `div` 2 ) ^ 2

-- Exercise 2
exercise2 = quickCheckWith stdArgs { maxSize = 20 } prop_subsequenceSize

prop_subsequenceSize :: [Integer] -> Bool
prop_subsequenceSize n =
  (^) 2 (genericLength n) == genericLength (subsequences n)

-- Exercise 3
exercise3 = quickCheckWith stdArgs { maxSize = 10 } solution3

solution3 (Positive n) = (length $ permutations [1..n]) == factorial(n)

factorial :: Int -> Int
factorial n  = product [1..n]

-- Exercise 4
exercise4 = print $ solution4

-- bit more efficient, instead of first generating the entire list,
-- now only create a list with the correct values on the fly
solution4 :: [Integer]
solution4 = [a | a <- [1..9999], primeReverse a]

-- helper method to check if reversal is also prime
primeReverse :: Integer -> Bool
primeReverse n = prime n && prime (reversal n)

reversal :: Integer -> Integer
reversal = read . reverse . show

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
-- Take the list of all primes up to the solution's prime
-- Compose all lists of 101 primes from this list
-- Validate that NO sum of these lists is a prime

exercise5 = print $ primesConSum

primesConSum :: Integer
primesConSum =  head $ filter (prime) $ map (primesSum 101) $ [0..]

primesSum :: Int -> Int -> Integer
primesSum x y = sum $ take x $ drop y primes

-- Exercise 6
exercise6 = print $ head listOfCounters

listOfCounters :: [[Integer]]
listOfCounters = [ take a primes | a <- [1..], let xs = take a primes, not $ prime $ 1 + product xs ]

-- Exercise 7

masterCards :: [Integer]
masterCards = [
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

visaCards :: [Integer]
visaCards = [
  4012888888881881,
  4111111111111111,
  4444333322221111,
  4911830000000,
  4917610000000000,
  4462030000000000,
  4917610000000000003
  ]

americanExpressCards :: [Integer]
americanExpressCards = [
  371449635398431,
  378282246310005
  ]

exercise7 = do
    putStr "Mastercard cards: "
    print $ all isMaster masterCards

    putStr "AmericanExpress cards: "
    print $ all isAmericanExpress americanExpressCards

    putStr "Visa cards: "
    print $ all isVisa visaCards

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress  = checkCardFormat [[34,37]] [15]
isMaster  = checkCardFormat [[51..55], [2221..2720]] [16]
isVisa  = checkCardFormat [[4]] [13, 16, 19]

checkCardFormat :: [[Integer]] -> [Integer] -> Integer -> Bool
checkCardFormat prefixRanges numberLength x = luhn x && (checkPrefixRanges x prefixRanges && elem (genericLength $ digits x) numberLength)

luhn :: Integer -> Bool
luhn x =  (luhnValue x )`mod` 10  == 0

luhnValue :: Integer -> Integer
luhnValue x = ((sum $ first $ reverseDigits x) +  (sumDoubleDigits $ map (*2) $ second $ reverseDigits x) )

first :: [a] -> [a]
first [] = []
first (x:xs) = x:second xs

second :: [a] -> [a]
second [] = []
second (x:xs) = first xs

sumDoubleDigits :: [Integer] -> Integer
sumDoubleDigits = sum . doubleDigits

doubleDigits :: [Integer] -> [Integer]
doubleDigits = map (sum . digits)

reverseDigits :: Integer -> [Integer]
reverseDigits =  reverse . digits

checkPrefixRanges :: Integer -> [[Integer]]  -> Bool
checkPrefixRanges x ranges  = or $ map(checkPrefixRange x) ranges

checkPrefixRange :: Integer -> [Integer]  -> Bool
checkPrefixRange x range = or $ map(checkPrefix x) $ map(digits) range

checkPrefix :: Integer -> [Integer] -> Bool
checkPrefix x y = and (zipWith (==) y (digits x))

-- Exercise 8
-- Encode everything as Haskell
-- Also provide base case when a boy doesn't mention all the boys otherwise we get a match error
-- We need three accusers to make someone guilty as 3 of them are speaking the truth
-- The boys who accused the guilty boy are honest
data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

exercise8 = do
  putStr "Guilty: "
  putStr $ show guilty
  putStr ", Honest: "
  putStrLn $ show honest

honest, guilty :: [Boy]
honest = concat [accusers x | x <- guilty]
guilty = [x | x <- boys, length (accusers x) == 3]

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

accusers :: Boy -> [Boy]
accusers x = [y | y <- boys, accuses y x]

accuses :: Boy -> Boy -> Bool
accuses Matthew n = n /= Carl && n /= Matthew
accuses Peter n = n == Matthew || n == Jack
accuses Jack n = not $ accuses Matthew n || accuses Peter n
accuses Arnold n = accuses Matthew n `xor` accuses Peter n
accuses Carl n = not $ accuses Arnold n

-- Bonus Exercises
exerciseBonus = do
  putStr "Project Euler #9: "
  print euler9
  putStr "Project Euler #10: "
  print euler10
  putStr "Project Euler #49: "
  print euler49

-- Euler 9
euler9 :: Integer
euler9 = head [a * b * c | a <- [1..1000], b <- [a..1000], let c = 1000 - a - b, a^2 + b^2 == c^2]

-- Euler 10
euler10 :: Integer
euler10 = sum $ takeWhile (< 2000000) primes

-- Euler 49
euler49 :: String
euler49 = result $ last $ filter isPermutation [(a , b, c) | a <- primes1000, let b = a + 3330, let c = b + 3330, b `elem` primes1000, c `elem` primes1000]

isPermutation :: (Integer, Integer, Integer) -> Bool
isPermutation (a, b, c) = elem (digits b) (permutations (digits a)) && elem (digits c) (permutations (digits a))

result :: (Integer, Integer, Integer) -> String
result (a, b, c) = (show a) ++ (show b) ++ (show c)

primes1000 :: [Integer]
primes1000 = dropWhile (< 1000) $ takeWhile (< 10000) primes

-- Utility functions used in multiple exercises
digits :: Integral x  => x -> [x]
digits 0 = []
digits x = digits (x `div` 10 ) ++ [x `mod` 10]

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