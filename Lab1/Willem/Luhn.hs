module Luhn where

luhn :: Integer -> Bool
luhn n = (flip mod 10 $ sumDigits $ doubleEveryOther $ reverse $ digits n) == 00

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = luhn n && length(digits n) == 15 && head (digits n) == 3 && (head (drop 1 (digits n)) == 7 || head (drop 1 (digits n)) == 4)
isMaster n = luhn n
isVisa n = head (digits n) == 4 && (length (digits n) == 15 || length (digits n) == 16 || length (digits n) == 17) && luhn n

isVisa1 :: [Integer] -> Bool
isVisa1 n@(4:_) = luhn n

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

