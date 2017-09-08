
-- Implementation time: 1 hour
-- Reason: not complete yet. It returs true, but it's not correct yet, since some random numbers also returned true.
-- Took the example from the wiki page, but the implementation is not completely clear.
-- Will read again tomorrow and work out. It appears that the last number should be simply compared.

main = print $ checkAccountNumber 7992739871

checkAccountNumber :: Int -> Bool
checkAccountNumber account = mod (summedValue account + returnCheckDigit account) 10 == 0

returnCheckDigit :: Int -> Int
returnCheckDigit = checkDigit . summedValue

summedValue = sum . smashList . doubleOdd . smash

checkDigit :: Int -> Int
checkDigit a = mod (9*a) 10

smashList :: [Int] -> [Int]
smashList [] = []
smashList (x : xs) | x >= 10 = (smash x) ++ smashList xs
                   | otherwise = x : smashList xs

doubleOdd :: [Int] -> [Int]
doubleOdd (x1:x2:xs) = (x1:2*x2:doubleOdd xs)
doubleOdd a = a

glue :: [Int] -> Int
glue xs = doGlue (reverse xs) 1

doGlue ::[Int] -> Int -> Int
doGlue [x] multiply = x * multiply
doGlue (x:xs) multiply = (multiply * x) + doGlue xs (multiply * 10)

smash :: Int -> [Int]
smash a = doSmash a []

doSmash :: Int -> [Int] -> [Int]
doSmash x xs | x < 10 = (x:xs)
           | otherwise = doSmash (div x 10) ((mod x 10) : xs)


