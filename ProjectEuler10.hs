-- Find the sum of all the primes below two million.

main = print $ sum $ addIfPrime [] 2

addIfPrime :: [Int] -> Int -> [Int]
addIfPrime a 2000000 = a 
addIfPrime a b = if isPrime b a then addIfPrime (b:a) (b+1) else addIfPrime a (b+1)

isPrime :: Int -> [Int] -> Bool
isPrime a b = False == divides a b

divides :: Int -> [Int] -> Bool 
divides a [] = False
divides a (b:bs) = (mod a b == 0) || divides a bs
