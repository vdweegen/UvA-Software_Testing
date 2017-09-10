import Prelude
import Test.QuickCheck

-- QuickCheck implementation of Workshop 1, exercise 2
-- time spent: 30 minutes
-- reason: Initial trial / error of QuickCheck

main :: IO()
main = do
  print "Checking Workshop 1, exercise 2"
  quickCheck prop_Exercise2
  print "Checking Workshop 1, exercise 3"
  quickCheck prop_Exercise3

prop_Exercise2 (Positive n) = summedSquaredListOfNumbers n == otherSquaredListOfNumbers n

summedSquaredListOfNumbers :: Integer -> Integer
summedSquaredListOfNumbers n = sum [ a^2 | a <- [0..n]]

otherSquaredListOfNumbers :: Integer -> Integer
otherSquaredListOfNumbers n = div (n * (n+1) * (2*n+1)) 6

prop_Exercise3 (Positive n) = summedThirdPowerListOfNumbers n == otherThirdPowerListOfNumbers n

summedThirdPowerListOfNumbers :: Integer -> Integer
summedThirdPowerListOfNumbers n = sum [ a^3 | a <- [0..n]]

otherThirdPowerListOfNumbers :: Integer -> Integer
otherThirdPowerListOfNumbers n = (div (n * (n+1)) 2)^2
