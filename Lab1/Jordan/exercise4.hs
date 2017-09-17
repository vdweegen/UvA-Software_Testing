import Test.QuickCheck
import Data.List
import Prelude

-- 40 minutes 
-- Writing a reverse function for the digits function was interesting. 


digits :: Integral x  => x -> [x]
digits 0 = []
digits x = digits (x `div` 10 ) ++ [x `mod` 10]

numbers :: [Integer] -> Integer
numbers [] = 0
numbers (x:xs) = x * (10 ^ (length xs)) + (numbers xs)

-- Given in lab
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 


-- take 10000 primes
isReversal x 
    | x < 13 = False
    | otherwise = (prime $ (numbers $ reverse (digits x))) && prime x

main = do 
    putStrLn "Primes with reversal property under 10000"
    print $ filter isReversal $ [1..10000]