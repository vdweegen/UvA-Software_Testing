import Prelude
import Test.QuickCheck

-- QuickCheck implementation of Workshop 1, exercise 2
main = quickCheck prop_Compare

prop_Compare (Positive n) = summedSquaredListOfNumbers n == otherSquaredListOfNumbers n

summedSquaredListOfNumbers :: Int -> Int
summedSquaredListOfNumbers n = sum $ map (^2) $ take n [1..]

otherSquaredListOfNumbers :: Int -> Int
otherSquaredListOfNumbers n = div (n * (n+1) * (2*n+1)) 6 
