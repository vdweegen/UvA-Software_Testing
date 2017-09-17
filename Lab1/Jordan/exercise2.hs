import Test.QuickCheck
import Data.List
import Prelude


-- An hour :(
-- Feels like I am testing my PC :P can't handle creating powersets of large sets.

cardinality :: [[Integer]] -> Integer
cardinality a = genericLength a

powerset :: [Integer] -> [[Integer]]
powerset = subsequences

toProveLength :: [Integer] -> Integer
toProveLength  xs = 2^(length xs)

workshop4 xs = ((2^(length xs)) == (cardinality $ powerset $  xs))

main = do
    putStrLn "Exercise 4 from the workshop in quickCheck format"
    quickCheckWith stdArgs { maxSize = 25 } workshop4