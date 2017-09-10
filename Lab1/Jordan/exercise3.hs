import Test.QuickCheck
import Data.List
import Prelude

-- 30 min

factorial n  = product [1..n]

workshop5 (Positive n) = (length $ permutations [1..n]) == factorial(n)

main = do
    putStrLn "Exercise 5 from the workshop in quickCheck format" 
    quickCheckWith stdArgs { maxSize=10 } workshop5