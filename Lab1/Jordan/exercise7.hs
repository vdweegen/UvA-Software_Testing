import Test.QuickCheck
import Data.List
import Prelude

digits :: Integral x  => x -> [x]
digits 0 = []
digits x = digits (x `div` 10 ) ++ [x `mod` 10]

numbers :: [Integer] -> Integer
numbers [] = 0
numbers (x:xs) = x * (10 ^ (length xs)) + (numbers xs)

-- 4255895263372077 test :)

first [] = []
first (x:xs) = x:second xs

second [] = []
second (x:xs) = first xs

reverseDigits =  reverse . digits
doubleDigits = map (sum . digits) 
sumDoubleDigits = sum . doubleDigits 

luhn x =  ((sum $ first $ reverseDigits x) +  (sumDoubleDigits $ map (*2) $ second $ reverseDigits x) ) `mod` 10 == 0


