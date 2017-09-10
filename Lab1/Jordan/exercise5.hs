import Test.QuickCheck
import Data.List
import Prelude

-- 20 min
-- This one was kind of easy. If you accept the fact that it will go into an infinite loop if you give it the wrong parameter :P 
-- I could try and see if I can solve it in a less naive way.


-- Given in lab
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

primesSum x y = sum $ take x $ drop y primes
primesconsum =  head $ filter (prime) $ map (primesSum 101) $ [0..]

main = do 
    print $ primesconsum 