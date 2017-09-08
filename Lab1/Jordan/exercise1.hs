import Test.QuickCheck
import Prelude

-- 3 days of reading xD
-- 15 minutes of coding :D

sumFirstNSquareNats :: Integer -> Integer
sumFirstNSquareNats n = sum [x ^ 2 | x <- [1..n]]

sumFirstNCubicNats :: Integer -> Integer
sumFirstNCubicNats n = sum [x ^ 3 | x <- [1..n]]

toProveSquaredNats :: Integer -> Integer
toProveSquaredNats n = (n * (n+1) * (2*n+1)) `div` 6

toProveCubedNats :: Integer -> Integer
toProveCubedNats n = ( n * (n+1) `div` 2) ^ 2

workshop2 (Positive n)  = sumFirstNSquareNats n == toProveSquaredNats n
workshop3 (Positive n)  = sumFirstNCubicNats n == toProveCubedNats n

main = do
    putStrLn "Exercise 2 from the workshop in quickCheck format"
    quickCheck workshop2
    putStrLn "Exercise 3 from the workshop in quickCheck format"
    quickCheck workshop3
