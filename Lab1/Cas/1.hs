module Lab1 where
import Data.List
import Text.Regex.Posix
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
    -- result1_1
    putStrLn $ "Assignment 1.2"
    -- result1_2
    putStrLn $ "Assignment 2"
    -- result2
    putStrLn $ "Assignment 3"
    -- result3
    putStrLn $ "Assignment 4"
    -- result4
    putStrLn $ "Assignment 5"
    -- TODO
    putStrLn $ "Assignment 6"
    -- TODO
    putStrLn $ "Assignment 7"
    result7
    putStrLn $ "Assignment 8"
    -- TODO
    putStrLn $ "BONUS"
    -- TODO

-- Assignment 1.1 --
base1_1 :: Integer -> Integer
induction1_1 :: Integer -> Integer
base1_1 = \n -> sum(map(^2)[0..n])
induction1_1 = \n -> (n*(n+1)*(2*n+1)) `div` 6
result1_1 = quickCheckResult(\n -> n >= 0 --> base1_1 n == induction1_1 n)

-- Assignment 1.2 --
base1_2 :: Integer -> Integer
induction1_2 :: Integer -> Integer
base1_2 = \n -> sum(map(^3)[0..n])
induction1_2 = \n -> ((n*(n+1)) `div` 2 ) ^ 2
result1_2 = quickCheckResult(\n -> n >= 0 --> base1_2 n == induction1_2 n)

-- Assignment 2 --
base2 :: Int -> Int
induction2 :: Int -> Int
base2 = \n -> 2^n
induction2 = \n -> length(subsequences[1..n])
result2 = quickCheckResult(\n -> n >= 0 --> induction2 n == base2 n)

-- Assignment 3 --
base3 :: Int -> Int
induction3 :: Int -> Int
base3 = \n -> length(permutations [1..n])
induction3 = \n -> product([1..n])
result3 = quickCheckResult(\n -> n >= 0 --> base3 n == induction3 n)

-- Assignment 4 --

-- TODO

-- Assignment 5 --

-- TODO

-- Assignment 6 --

-- TODO

-- Assignment 7 --
toDigits :: Integer -> [Integer]
toDigits n = map (\x -> read [x] :: Integer) (show n)

toRevDigits :: Integer -> [Integer]
toRevDigits n = if n <= 0 then []
  else mod n 10 : toRevDigits (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (n1 : n2 : ns) = n1 : 2 * n2 : doubleEveryOther ns
doubleEveryOther catch = catch

sumDigits :: [Integer] -> Integer
sumDigits (x:xs) = sum(toRevDigits x) + sumDigits xs
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

-- Assignment 8 --

-- TODO
