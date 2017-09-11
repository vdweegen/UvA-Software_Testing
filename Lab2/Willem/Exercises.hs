module Lab2 where

import Data.List
import Test.QuickCheck
import Data.Bits
import System.Random
import Control.Monad
import Control.Applicative

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Exercise 1
-- About one hour also to think of chi test
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

quartile :: Float -> Float -> Float -> Bool
quartile min max n = n >= min && n < max

quartile1, quartile2, quartile3, quartile4 :: Float -> Bool
quartile1 n = quartile 0 0.25 n
quartile2 n = quartile 0.25 0.5 n
quartile3 n = quartile 0.5 0.75 n
quartile4 n = quartile 0.75 1 n

chi :: Int -> Int -> Float
chi x m = fromIntegral((x-m)^2) / fromIntegral m


distribution :: Int -> IO ()
distribution n = do
                    p <- probs n
                    let a = length $ filter quartile1 p
                    let b = length $ filter quartile2 p
                    let c = length $ filter quartile3 p
                    let d = length $ filter quartile4 p
                    let m = div n 4
                    let x = (chi a m) + (chi b m) + (chi c m) + (chi d m)
                    print (a,b,c,d,x)

-- Exercise 2
data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = NoTriangle

noTriangle, equilateral, isosceles, rectangular, other :: Integer -> Integer -> Integer -> Bool
noTriangle a b c = ((a+b) >= c) || ((a+c) >= b) || ((b+c) >= a)

equilateral a b c = True
isosceles a b c = True
rectangular a b c = True
other a b c = True
