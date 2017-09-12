module Exercises where
import Data.List

import Lab2.Util.Random

main = do
        putStr "Distribution of 10000 random values: "
        exercise1

exercise1 = probs 10000 >>= (\list -> return $ isEvenlyDistributed list)

isEvenlyDistributed :: [Float] -> (Integer, Integer, Integer, Integer)
isEvenlyDistributed = sumTuples . categorize

categorize :: [Float] -> [(Integer, Integer, Integer, Integer)]
categorize fs = [ b | a <- fs, let b = createTuple a]

createTuple :: Float -> (Integer, Integer, Integer, Integer)
createTuple float | float < 0.25 = (1,0,0,0)
                  | float < 0.50 = (0,1,0,0)
                  | float < 0.75 = (0,0,1,0)
                  | otherwise = (0,0,0,1)

sumTuples :: [(Integer, Integer, Integer, Integer)] -> (Integer,Integer,Integer,Integer)
sumTuples xs = doSum xs (0,0,0,0)

doSum :: [(Integer, Integer, Integer, Integer)] -> (Integer,Integer,Integer,Integer) -> (Integer,Integer,Integer,Integer)
doSum [] a = a
doSum ((a1,a2,a3,a4):xs) (b1,b2,b3,b4) = doSum xs (a1+b1, a2+b2, a3+b3, a4+b4)




