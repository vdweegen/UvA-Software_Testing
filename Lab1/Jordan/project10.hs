-- Given in lab
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

main = do 
    print $ sum $ takeWhile(<2000000) primes