module Workshop6 where
import Data.Char
import Data.List

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

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T n []) = T (f n) []
mapT f (T n ts) = T (f n) (map (mapT f) ts)

example1MapT, example2MapT :: Tree Int
example1MapT = mapT succ example1
example2MapT = mapT succ example2

collect :: Tree a -> [a]
collect (T n []) = [n]
collect (T n ts) = n : (concatMap collect ts)

foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T x ts) = f x (map (foldT f) ts)

count' :: Tree a -> Int
count' = foldT (\_ ts -> 1 + sum ts)

depth' :: Tree a -> Int
depth' ts = foldT (\_ xs -> foldl max 0 xs + 1) ts - 1

collect' :: Tree a -> [a]
collect' ts = error ""

mapT' :: (a -> b) -> Tree a -> Tree b
mapT' f xs = foldT (fts) xs


fts :: a -> [b] -> b
fts = error ""
--map f = foldr ((:).f) []
--map' f = foldr (\x xs -> f x : xs) []
--filter' p = foldr (\x xs -> if p x then x : xs else xs) []


grow :: (node -> [node]) -> node -> Tree node
grow step seed = T seed (map (grow step) (step seed))

infTree :: Tree Integer
infTree = grow (\ n -> [n+1,n+1]) 0

takeT :: Int -> Tree a -> Tree a
takeT = error "not yet implemented"

tree n = grow (f n) (1,1)
f n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else []

instance Arbitrary a => Arbitrary (Tree a)