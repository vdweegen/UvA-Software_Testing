module Lab2 where

import Data.List
import Data.Char
import System.Random
import System.IO.Unsafe
import Test.QuickCheck
import Control.Monad (replicateM)

-- Assignment 2 / Lab 2 :: Group 14 --

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Define Main --
main = do
    putStrLn $ "===================="
    putStrLn $ "Assignment 2 / Lab 2"
    putStrLn $ "===================="
    putStrLn $ "> Exercise 1"
    exercise1
    putStrLn $ "> Exercise 2"
    exercise2
    putStrLn $ "> Exercise 3a"
    exercise3a
    putStrLn $ "> Exercise 3b"
    exercise3b
    putStrLn $ "> Exercise 4"
    exercise4
    putStrLn $ "> Exercise 5"
    exercise5
    putStrLn $ "> Exercise 6"
    exercise6
    putStrLn $ "> Exercise 7"
    exercise7
    putStrLn $ "> BONUS"
    exercisebonus

-- Exercise 1 :: Spent Time: +-30min
exercise1 = solution1

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n-1)
  return (p:ps)

filterLower :: Float -> [Float] -> [Float]
filterHigher :: Float -> [Float] -> [Float]
filterLower n list = filter (\x -> x>n) list
filterHigher n list = filter (\x -> x<=n) list

solution1 = do
  numbers <- probs 10000
  let q1 = intersect (filterLower 0.0 numbers) (filterHigher 0.25 numbers)
  let q2 = intersect (filterLower 0.25 numbers) (filterHigher 0.50 numbers)
  let q3 = intersect (filterLower 0.50 numbers) (filterHigher 0.75 numbers)
  let q4 = intersect (filterLower 0.75 numbers) (filterHigher 1.00 numbers)
  print (length q1)
  print (length q2)
  print (length q3)
  print (length q4)


-- Exercise 2 :: Spent Time: +-90 minutes (plus an additional 120 minutes for testing)
exercise2 = solution2

data Shape = NoTriangle | Equilateral
  | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
  | x + y <= z || x + z <= y || y + z <= x = NoTriangle
  | x == y && y == z && z == y = Equilateral
  | x == y || x == z || y == z = Isosceles
  | x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || y^2 + z^2 == x^2 = Rectangular
  | otherwise = Other

combtriangle :: [Integer] -> Shape
combtriangle x = triangle (x !! 0) (x !! 1) (x !! 2)

randomNoTriangle :: Integer -> IO [Integer]
randomNoTriangle n = do
  a <- drawInt 1 n
  b <- drawInt 1 n
  if a == b then do
    randomNoTriangle n
  else
    return [a,b,n*2]

randomEquilateral :: Integer -> IO [Integer]
randomEquilateral n = do
  a <- drawInt 1 n
  return [a,a,a]

randomIsosceles :: Integer -> IO [Integer]
randomIsosceles n = do
  a <- drawInt 1 n
  if a == 1 then do
    randomIsosceles n
  else do
    return [a,a,a+1]

randomRectangular :: Integer -> IO [Integer]
randomRectangular n = do
  a <- drawInt 1 n
  return [3*a,4*a,5*a]

-- In this case we should ignore n
randomOther :: Integer -> IO [Integer]
randomOther n = do
  a <- drawInt 1 50
  return [51*a,55*a,5*a]

drawInt :: Integer -> Integer -> IO Integer
drawInt x y = getStdRandom (randomR (x,y))

testTriangle :: Integer -> ([Integer] -> Shape) -> IO [Integer]
    -> (Shape -> Bool) -> IO ()
testTriangle n f i p = test 1 n f i (\_ -> p)

test :: Integer -> Integer -> ([Integer] -> Shape) -> IO [Integer]
    -> ([Integer] -> Shape -> Bool) -> IO ()
test k n f i r =
  if k == n then
    print (show n ++ " tests passed")
  else do
    xs <- i
    if r xs (f xs) then
      do test (k+1) n f i r
    else error (show xs ++ " failed after " ++ (show k) ++ " attempts")

prop :: Shape -> Shape -> Bool
prop shape target | shape == target = True | otherwise = False

solution2 = do
  putStrLn "NoTriangle:"
  testTriangle 100 combtriangle (randomNoTriangle 100) (prop NoTriangle)
  putStrLn "Equilateral:"
  testTriangle 100 combtriangle (randomEquilateral 100) (prop Equilateral)
  putStrLn "Isosceles:"
  testTriangle 100 combtriangle (randomIsosceles 100) (prop Isosceles)
  putStrLn "Rectangular:"
  testTriangle 100 combtriangle (randomRectangular 100) (prop Rectangular)
  putStrLn "Other:"
  testTriangle 100 combtriangle (randomOther 100) (prop Other)

-- Exercise 3a :: Spent Time: +-60 minutes
exercise3a = solution3a

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

one, two, three, four :: Int -> Bool
one = (\x -> even x && x > 3)
two = (\x -> even x || x > 3)
three = (\x -> (even x && x > 3) || even x)
four = (\x -> (even x && x > 3) || even x)

domain :: [Int]
domain = [-10..10]

data PropertyStrength = Stronger | Weaker | Equivalent | Incomparable
  deriving (Eq, Show)

compar :: [a] -> (a -> Bool) -> (a -> Bool) -> PropertyStrength
compar xs p q
  | (stronger xs p q) && (stronger xs q p) = Equivalent
  | stronger xs p q = Stronger
  | stronger xs q p = Weaker
  | otherwise = Incomparable

-- permcompar x (y,z) = compar x y z

combcompar x y = compar x (y !! 0) (y !! 1)

instance Ord PropertyStrength where
  compare Stronger Stronger = EQ
  compare Stronger Weaker = GT
  compare Stronger Equivalent = GT
  compare Stronger Incomparable = GT
  compare Equivalent Stronger = LT
  compare Equivalent Equivalent = EQ
  compare Equivalent Weaker = GT
  compare Equivalent Incomparable = GT
  compare Weaker Stronger = LT
  compare Weaker Equivalent = LT
  compare Weaker Weaker = EQ
  compare Weaker Incomparable = GT
  compare Incomparable Stronger = LT
  compare Incomparable Equivalent = LT
  compare Incomparable Weaker = LT
  compare Incomparable Incomparable = EQ

-- props = [(one,two),(one,three),(one,four),(two,three),(three,four)]

solution3a = do
  print $ compar domain one two
  print $ compar domain one three
  print $ compar domain one four
  print $ compar domain two three
  print $ compar domain three four

-- Exercise 3b :: Spent Time: +-60 minutes
exercise3b = solution3b

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

solution3b = do
  -- print $ sort $ map (permcompar domain) props
  print $ sort $ map (combcompar domain) (combinations 2 [one,two,three,four])


-- Exercise 4 :: Spent Time: +-30 minutes
exercise4 = solution4

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = elem x (permutations y)

solution4 = do
  print $ isPermutation [3,2,1] [1,2,3]

-- Exercise 5 :: Spent Time: +-30 minutes
exercise5 = solution5

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement x y = ((length $ findIndices id $ zipWith (==) x y) == 0) && isPermutation x y

deran :: Eq a => [a] -> [[a]]
deran x = filter (\ y -> isDerangement y x) (permutations x)

solution5 = do
  print $ isDerangement [3,2,1] [1,2,3]
  print $ deran [1,2,3]

-- Exercise 6 :: Spent Time: +-30 minutes
exercise6 = solution6

rotify :: Int -> Int
rotify x
  | (x <= 77) && (x >= 65) = (x + 13)
  | (x <= 90) && (x >= 78) = (x - 13)
  | (x <= 109) && (x >= 97) = (x + 13)
  | (x <= 122) && (x >= 110) = (x - 13)
  | otherwise = x

rot13 :: [Char] -> [Char]
rot13 x = map chr (map rotify (map ord x))

randomStr :: Int -> String
randomStr n = take n $ randomRs ('a','z') $ unsafePerformIO newStdGen

prop_rot13 :: String -> Bool
prop_rot13 s | s == rot13 (rot13 s) = True | otherwise = False

result_prop_rot13 = do
  quickCheckResult(\n -> n >= 1 --> (prop_rot13 (rot13 (randomStr n))) == True)

solution6 = do
  result_prop_rot13
  -- print $ rot13 "Why is it we are here?"
  -- print $ rot13 "There are 26 letters in the alphabet!"
  -- print $ rot13 "Jul vf vg jr ner urer?"
  -- print $ rot13 "Gurer ner 26 yrggref va gur nycunorg!"

-- Exercise 7 :: Spent Time: +-120 minutes
exercise7 = solution7

-- validate length
ibanValidLength :: String -> Bool
ibanValidLength x = length x >= 4 && length x <= 38

-- move first four characters to end of string
ibanRearrange :: String -> String
ibanRearrange (a:b:c:d:x) = x ++ [a,b,c,d]
ibanRearrange _ = error "INVALID!"

-- replace letters with digits (A = 10, .., Z = 35)
ibanConvertToDecimal :: Int -> [Char]
ibanConvertToDecimal x
  | (x >= 65) && (x <= 90) = show(x - 55)
  | (x >= 97) && (x <= 122) = show(x - 87)
  | x == 32 = ""
  | otherwise = [chr x]

ibanTransform :: String -> String
ibanTransform x = intercalate "" (map ibanConvertToDecimal (map ord (ibanRearrange (filter (/=' ') x))))

-- calculate mod 97, remainder should be 1
iban :: String -> Bool
iban x = ((mod (read (ibanTransform x) :: Integer) 97) == 1) && ibanValidLength x

shuffleNums :: Int -> Int
shuffleNums x
  | (x < 57) && (x >= 48) = (x + 1)
  | x == 57 = (x - 9)
  | otherwise = x

invalidateIban :: String -> String
invalidateIban x = map chr (map shuffleNums (map ord x))

solution7 = do
  print $ iban "NL13ABNA2859176594"
  print $ iban (invalidateIban "NL13ABNA2859176594")
  print $ iban "NL51ABNA5993159871"
  print $ iban (invalidateIban "NL51ABNA5993159871")
  print $ iban "KW78FNGG8776167638766772197428"
  print $ iban (invalidateIban "KW78FNGG8776167638766772197428")
  print $ iban "DE78500105173914769993"
  print $ iban (invalidateIban "DE78500105173914769993")
  print $ iban "BG06RZBB91558151154695"
  print $ iban (invalidateIban "BG06RZBB91558151154695")
  print $ iban "CY37164286132581834641882141"
  print $ iban (invalidateIban "CY37164286132581834641882141")
  print $ iban "IS591315989522712263483388"
  print $ iban (invalidateIban "IS591315989522712263483388")
  print $ iban "MR5739674777488275245272698"
  print $ iban (invalidateIban "MR5739674777488275245272698")
  print $ iban "PL91109024023719463892436661"
  print $ iban (invalidateIban "PL91109024023719463892436661")
  print $ iban "MD9517694539799293438398"
  print $ iban (invalidateIban "MD9517694539799293438398")
  print $ iban "LC03ZJWP277836622529389688132565"
  print $ iban (invalidateIban "LC03ZJWP277836622529389688132565")
  print $ iban "AE260212564146133481178"
  print $ iban (invalidateIban "AE260212564146133481178")

-- Bonus Exercises
exercisebonus = print()
