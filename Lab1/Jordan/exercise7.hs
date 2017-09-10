import Test.QuickCheck
import Data.List
import Prelude

-- -/+ 1 hour But thought about the problem for 2 days before attempting to solve it in Haskell. 

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
 5555555555554444]

visas = [4012888888881881,
 4111111111111111,
 4444333322221111,
 4911830000000,
 4917610000000000,	
 4462030000000000,
 4917610000000000003]

americanExpress = [
 371449635398431,
 378282246310005]

main = do 
    putStrLn "Mastercard cards"
    print $ all isMaster mastercards   

    putStrLn "AmericanExpress cards"
    print $ all isAmericanExpress americanExpress   

    putStrLn "Visa cards"
    print $ all isVisa visas  
