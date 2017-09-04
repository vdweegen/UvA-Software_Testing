import Prelude
import Test.QuickCheck

main = quickCheck prop_Compare

prop_Compare (Positive n) =
  summedListOfNumbers n == otherListOfNumbers a

summedListOfNumbers :: Int -> Int
summedListOfNumbers amount = sum $ take amount [1..]

otherListOfNumbers :: Int -> Int
otherListOfNumbers amount = div (amount * (amount + 1)) 2 
