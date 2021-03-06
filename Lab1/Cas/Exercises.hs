module Lab1 where
import Data.List
import Test.QuickCheck

-- Lab Week 1 --

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
    putStrLn $ "Assignment 1.1"
    result1_1
    putStrLn $ "Assignment 1.2"
    result1_2
    putStrLn $ "Assignment 2"
    -- result2
    putStrLn $ "Assignment 3"
    -- result3
    putStrLn $ "Assignment 4"
    result4
    putStrLn $ "Assignment 5"
    result5
    putStrLn $ "Assignment 6"
    result6
    putStrLn $ "Assignment 7"
    result7
    putStrLn $ "Assignment 8"
    result8
    putStrLn $ "BONUS"
    -- TODO

-- Assignment 1.1 :: time spent:  +- 45 minutes --
basecase1 :: Integer -> Integer
inductioncase1 :: Integer -> Integer
basecase1 n = sum [ a^2 | a <- [0..n]]
inductioncase1 n = (n*(n+1)*(2*n+1)) `div` 6
result1_1 = quickCheckResult(\n -> n >= 0 --> basecase1 n == inductioncase1 n)

-- Assignment 1.2 :: time spent: +- 45 minutes --
basecase2 :: Integer -> Integer
inductioncase2 :: Integer -> Integer
basecase2 n = sum [ a^3 | a <- [0..n]]
inductioncase2 n = ((n*(n+1)) `div` 2 ) ^ 2
result1_2 = quickCheckResult(\n -> n >= 0 --> basecase2 n == inductioncase2 n)

-- Assignment 2 :: time spent: +- 30 minutes --
basecase3 :: Integer -> Integer
inductioncase3 :: Integer -> Integer
basecase3 n = 2^n
inductioncase3 n = genericLength(subsequences[1..n])
result2 = quickCheckResult(\n -> n >= 0 --> basecase3 n == inductioncase3 n)

-- Assignment 3 :: time spent: +- 60 minutes --
basecase4 :: Integer -> Integer
inductioncase4 :: Integer -> Integer
basecase4 n = genericLength(permutations [1..n])
inductioncase4 n = product([1..n])
result3 = quickCheckResult(\n -> n >= 0 --> basecase4 n == inductioncase4 n)

-- Assignment 4 :: time spent: +- 30 minutes --
reversal :: Integer -> Integer
reversal = read . reverse . show

emirps :: [Integer]
emirps = [p | p <- primes, let rev = reversal p, prime rev, p /= rev]

result4 = do
  print $ takeWhile (< 10000) emirps

-- Assignment 5 :: time spent: +- 60 minutes --
consecutive :: [Integer] -> Integer
consecutive n
    | prime m = m
    | otherwise = consecutive (tail n)
    where m = sum(take 101 n)

result5 = do
  print (consecutive (takeWhile (<10000) primes))

-- Assignment 6 :: time spent: +- 10 minutes --
result6 = quickCheckResult(\n -> n > 2 --> prime (sum(takeWhile (< n) primes) + 1))

-- Assignment 7 :: time spent: 120 minutes --
toDigits :: Integer -> [Integer]
toDigits n = map (\x -> read [x] :: Integer) (show n)

toRevDigits :: Integer -> [Integer]
toRevDigits n = if n <= 0 then []
  else mod n 10 : toRevDigits (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (n1 : n2 : m) = n1 : 2 * n2 : doubleEveryOther m
doubleEveryOther catch = catch

sumDigits :: [Integer] -> Integer
sumDigits (x:y) = sum(toRevDigits x) + sumDigits y
sumDigits [] = 0

luhn :: Integer -> Bool
luhn n = (sumDigits $ doubleEveryOther
           $ toRevDigits n)
           `mod` 10 == 0

-- AMEX: Start with a 0, second is 4 or 7, length is 15
isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = let d = (toDigits n) in
  d !! 0 == 3 && ((d !! 1 == 4) || d !! 1 == 7) && length d == 15 && luhn n

-- Master: pain in the a**
-- OR
-- (1) 5[1-5][0-9]{14}
-- (2) 2(22[1-9][0-9]{12}
-- (3) 2[3-9][0-9]{13}
-- (4) [3-6][0-9]{14}
-- (5) 7[0-1][0-9]{13}
-- (6) 720[0-9]{12}
isMaster n = let d = (toDigits n) in
  (d !! 0 == 5 && ((d !! 1 == 1) || (d !! 1 == 2) || (d !! 1 == 3) || (d !! 1 == 4) || (d !! 1 == 5)) && length d == 16) || -- (1)
  (d !! 0 == 2 && d !! 1 == 2 && d !! 2 == 2 && d !! 3 /= 0 && length d == 16) || -- (2)
  (d !! 0 == 2 && d !! 1 /= 0 && d !! 1 /= 1 && d !! 1 /= 2 && length d == 15) || -- (3)
  (d !! 0 /= 0 && d !! 0 /= 1 && d !! 0 /= 2 && d !! 0 /= 7 && d !! 0 /= 8 && d !! 0 /= 9 && length d == 15) || -- (4)
  (d !! 0 == 7 && (d !! 0 == 0 || d !! 0 == 1) && length d == 15) || -- (5)
  (d !! 0 == 7 && d !! 1 == 2 && d !! 2 == 0 && length d == 15) && -- (6)
  luhn n

-- VISA: Start with a 4, end either 13 or 16 long
isVisa n = let d = (toDigits n) in
  d !! 0 == 4 && (length d == 13 || length d == 16) && luhn n

result7 = do
  print("Check Visa (VALID CARD) 4024007187018576")
  print(isVisa 4024007187018576)
  print("Check Visa (INVALID CARD) 4034007187018576")
  print(isVisa 4034007187018576)
  print("Check AMEX (VALID CARD) 379603918985031")
  print(isAmericanExpress 379603918985031)
  print("Check AMEX (INVALID CARD) 379403918985031")
  print(isAmericanExpress 379403918985031)
  print("Check Master (VALID CARD) 5319329243834379")
  print(isMaster 5319329243834379)
  print("Check Master (INVALID CARD) 53029243834379")
  print(isMaster 53029243834379)

-- Assignment 8 :: time spent: 45 minutes --
data Boy = Matthew | Peter | Jack | Arnold | Carl
  deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew n = (n /= Carl && n /= Matthew)
accuses Peter n = (n == Matthew || n == Jack)
accuses Jack n = not(accuses Matthew n || accuses Peter n)
accuses Arnold n = (accuses Matthew n || accuses Peter n) && not(accuses Matthew n && accuses Peter n)
accuses Carl n = not(accuses Arnold n)

accusers :: Boy -> [Boy]
accusers x = [y | y <- boys, accuses y x]

guilty, honest :: [Boy]
-- 3 tell the truth (so we check who is accused exactly 3 times)
guilty = [x | x <- boys, length (accusers x) == 3]
-- 2 always lie (find out who accused the guilty boy and didn't accuse the innocent)
honest = nub [x | x <- boys, y <- guilty, z <- (boys \\ guilty), accuses x y && not (accuses x z)]

result8 = do
  print "Guilty"
  print guilty
  print "Honest"
  print honest
