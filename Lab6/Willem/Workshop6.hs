module Workshop6 where
import Data.Char
import Data.List
import Data.List
import Test.QuickCheck
import System.Random
import Control.Monad

data Blt a = Leaf a | Node (Blt a) (Blt a) deriving (Eq,Show)

exampleTree :: Blt String
exampleTree = Node (Node (Leaf "Hoare, Tony")
                         (Leaf "Turing, Alan"))
                   (Leaf "Goedel, Kurt")

leafCount :: Blt a -> Int
leafCount (Leaf _) = 1
leafCount (Node left right) = leafCount left + leafCount right

exampleCount :: Int
exampleCount = leafCount exampleTree

mapB :: (a -> b) -> Blt a -> Blt b
mapB f (Leaf leaf) = Leaf (f leaf)
mapB f (Node left right) = Node (mapB f left) (mapB f right)

exampleMapB :: Blt String
exampleMapB = mapB (map toUpper) exampleTree

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

example1 :: Tree Int
example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]

count :: Tree a -> Int
count (T _ []) = 1
count (T _ ts) = sum (map count ts) + 1

depth :: Tree a -> Int
depth (T _ []) = 0
depth (T _ ts) = foldl max 0 (map depth ts) + 1

depthR :: Tree a -> Int
depthR (T _ []) = 0
depthR (T _ ts) = 1 + maximum (map depthR ts)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T n []) = T (f n) []
mapT f (T n ts) = T (f n) (map (mapT f) ts)

example1MapT, example2MapT :: Tree Int
example1MapT = mapT succ example1
example2MapT = mapT succ example2

collect :: Tree a -> [a]
collect (T n []) = [n]
collect (T n ts) = n : (concatMap Workshop6.collect ts)

foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T x ts) = f x (map (foldT f) ts)

count' :: Tree a -> Int
count' = foldT (\_ ts -> 1 + sum ts)

depth' :: Tree a -> Int
depth' =  minus1 . (foldT (\_ xs -> 1 + foldl max 0 xs))

depth'' :: Tree a -> Int
depth''  = foldT (\_ xs -> max 0 (foldr (max.(+1)) 0 xs))

minus1 :: Integer -> Int
minus1 a = fromIntegral(a - 1)

collect' :: Tree a -> [a]
collect' = foldT (\t ts -> t : concat ts)

mapT' :: (a -> b) -> Tree a -> Tree b
mapT' f = foldT (\t ts -> T (f t) ts)

grow :: (node -> [node]) -> node -> Tree node
grow step seed = T seed (map (grow step) (step seed))

infTree :: Tree Integer
infTree = grow (\ n -> [n+1,n+1]) 0

takeT :: Int -> Tree a -> Tree a
takeT = error "not yet implemented"

tree n = grow (f n) (1,1)
f n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else []

instance (Arbitrary a, Num a) => Arbitrary (Tree a) where
  arbitrary = sized $ \n -> do
     k <- choose (0, n)
     t <- arbitrary
     ts <- sequence [arbitrary | _ <- [0..k `div` 8]]
     return (T t (drop 1 ts))

  shrink (T t ts) = T t `liftM` shrink ts

prop_depth :: Tree Int -> Bool
prop_depth t = depth t == depth' t