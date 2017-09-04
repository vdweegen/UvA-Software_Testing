import Prelude
import Test.QuickCheck

main = quickCheck prop_Compare_2
       
prop_Compare_1 (Positive n) =
  summedListOfNumbers n == otherListOfNumbers n

prop_Compare_2 (Positive n) =
  summedSquaredListOfNumbers n == otherSquaredListOfNumbers n

summedListOfNumbers :: Int -> Int
summedListOfNumbers n = sum $ take n [1..]

otherListOfNumbers :: Int -> Int
otherListOfNumbers n = flip div 2 $ (*) n $ n + 1

summedSquaredListOfNumbers :: Int -> Int
summedSquaredListOfNumbers n = sum $ map (^2) $ take n [1..]

otherSquaredListOfNumbers :: Int -> Int
otherSquaredListOfNumbers n = div (n * (n+1) * (2*n+1)) 6 
