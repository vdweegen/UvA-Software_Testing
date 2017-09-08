import Data.List
import Test.QuickCheck

-- implementation time: 5 minutes
-- debugging time: 60 minutes
-- reason: same as previous exercise
-- Note, will throw a stack overflow whilst testing => Requires resizing of list
-- Finding out the correct use of the quickCheck (quickCheckWith) took a lot of digging and trying

-- What are we testing actually?
-- We are testing whether the amount of lists matches the expected amount, the content is not checked

-- Are you checking a mathematical fact or whether subsequences matches a part of its specification or something else?

main = quickCheckWith stdArgs { maxSize=10 } prop_permutationSize

prop_permutationSize :: [Integer] -> Bool
prop_permutationSize xs =
  factorial (genericLength xs) == genericLength (permutations xs)

factorial :: Integer -> Integer
factorial n = foldl (*) 1 [1..n]

