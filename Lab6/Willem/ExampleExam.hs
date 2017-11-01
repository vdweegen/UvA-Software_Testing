module ExamOct2016 where
import Data.List
import Test.QuickCheck

type Rel a = [(a,a)]

isAntiSymm :: Eq a => Rel a -> Bool
isAntiSymm r = error "not yet implemented"

data HTree = Leaf Char Int | Fork HTree HTree Int deriving (Show)

weight :: HTree -> Int
weight (Leaf _ w) = w
weight (Fork _ _ w) = w

prop_huffman :: HTree -> Bool
prop_huffman (Leaf _ _) = True
prop_huffman (Fork t1 t2 w) = prop_huffman t1 && prop_huffman t2 && weight t1 + weight t2 == w

createTree :: [(Char,Int)] -> HTree
createTree [] = error "empty input list"
createTree [(c,i)] = Leaf c i
createTree ((c,i):t) = merge (Leaf c i) (createTree t)

merge :: HTree -> HTree -> HTree
merge t1 t2 = Fork t1 t2 (weight t1 + weight t2)

freqList :: String -> [(Char,Int)]
freqList s = error "not yet implemented"

string2tree :: String -> HTree
string2tree = createTree . freqList

probSuccess1 :: Int -> Rational -> Rational
probSuccess1 k p = let q = 1 - p in sum [ p * q^m | m <- [1..k-1] ]

probSuccess2 :: Int -> Rational -> Rational
probSuccess2 k p = 1 - (1 - p)^k

probSuccess3 :: Int -> Rational -> Rational
probSuccess3 k p = let q = 1 - p in ((q^k - 1) / (q - 1)) * p

russianRoulette :: Rational
russianRoulette = 1/6 + (5/6)*(1/6) + (5/6)^2*(1/6) + (5/6)^3*(1/6) + (5/6)^4*(1/6) + (5/6)^5*(1/6)

prop_equal :: (Int, Rational) -> Bool
prop_equal (p, r) = probSuccess1 p r == probSuccess2 p r && probSuccess2 p r == probSuccess3 p r
