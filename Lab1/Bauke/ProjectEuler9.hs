-- ProjectEuler assignment 9
-- find the special Pythagorean triples for which a + b + c = 1000

-- Since the sum of the triplets is 1000, the maximum value of c can also be 1000.
-- Since it's a natural number, a^2 + b^2 should be in the list of squared values from 0 .. 1000
-- Takes some time, but resolves to 31875000 

main = print $ findTriplet 1 2

findTriplet :: Int -> Int -> Int
findTriplet 1000 _ = -1
findTriplet a b | (a + b) > 1000 = findTriplet (a+1) (a+2)
                | triplet a b = a * b * (calcSide a b)
                | otherwise = findTriplet a (b+1)

triplet :: Int -> Int -> Bool
triplet a b = (isSquare (calcSquared a b)) && (sumValid a b (calcSide a b))

calcSide :: Int -> Int -> Int
calcSide a b = floor $ sqrt $ fromIntegral $ calcSquared a b

calcSquared :: Int -> Int -> Int 
calcSquared a b = (a^2 + b^2)

isSquare :: Int -> Bool
isSquare a = contains possibleSquares a

contains :: [Int] -> Int -> Bool
contains [] a = False
contains (x:xs) a | a == x = True
                  | otherwise = contains xs a

possibleSquares :: [Int]
possibleSquares = squares 1000

squares :: Int -> [Int]
squares a = take a $ map (^2) [1..] 

sumValid :: Int -> Int -> Int -> Bool
sumValid a b c = a + b + c == 1000
