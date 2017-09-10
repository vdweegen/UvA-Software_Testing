module Luhn where

-- Exercise 7: Helper methods to doubleEveryOther, sumDigits
-- and to split a number in to an array with digits
-- source: https://en.wikipedia.org/wiki/Payment_card_number
-- source: https://en.wikipedia.org/wiki/Luhn_algorithm
-- Time spent: 30 min

luhn :: Integer -> Bool
luhn n = (flip mod 10 $ sumDigits $ doubleEveryOther $ reverse $ digits n) == 00

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = luhn n
                    && length(digits n) == 15
                    && digits n !! 0 == 3
                    && (digits n !! 1 == 7 || digits n !! 1 == 4)

isMaster n = luhn n
           && length(digits n) == 16
           && ((digits n !! 0 == 5) && ((digits n !! 1 >= 1) && (digits n !! 1 <= 5)))

isVisa n = luhn n
         && head (digits n) == 4
         && (length (digits n) == 15 || length (digits n) == 16 || length (digits n) == 17)


doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:z) = x:y*2:doubleEveryOther z

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:xs) | x < 10 = x + sumDigits xs
                 | otherwise = x - 9 + sumDigits xs

digits :: Integer -> [Int]
digits = map (read . return) . show
