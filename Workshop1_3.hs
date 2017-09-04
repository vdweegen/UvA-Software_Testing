import Prelude
import Test.QuickCheck

-- QuickCheck implementation of Workshop 1, exercise 3 
-- time spent: < 10 minutes
-- reason: Nearly identical to Previous exercise

main = quickCheck prop_Compare

prop_Compare (Positive n) = summedThirdPowerListOfNumbers n == otherThirdPowerListOfNumbers n

summedThirdPowerListOfNumbers :: Int -> Int
summedThirdPowerListOfNumbers n = sum $ map (^3) $ take n [1..]

otherThirdPowerListOfNumbers :: Int -> Int
otherThirdPowerListOfNumbers n = (div (n * (n+1)) 2)^2
