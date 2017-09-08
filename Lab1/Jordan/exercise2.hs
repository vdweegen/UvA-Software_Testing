import Test.QuickCheck
import Data.List
import Prelude


-- An hour :(
-- Feels like I am testing my PC :P can't handle creating powersets of large sets.

cardinality :: [[Int]] -> Int
cardinality a = length a

powerset :: [Int] -> [[Int]]
powerset = subsequences

toProveLength :: [Int] -> Int
toProveLength  xs = 2^(length xs)

workshop4 xs = (toProveLength xs == (cardinality $ powerset $  xs))

main = do
    putStrLn "Exercise 4 from the workshop in quickCheck format"
    quickCheck workshop4