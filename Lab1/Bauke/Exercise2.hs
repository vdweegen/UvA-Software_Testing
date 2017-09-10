import Data.List
import Test.QuickCheck

-- implementation time: 15 minutes
-- reason: looking up quickCheck for lists and length vs genericLength
-- Note, will throw a stack overflow whilst testing => Requires resizing of list

-- What are we testing actually?
-- We are testing whether the amount of lists matches the expected amount, the content is not checked

-- Are you checking a mathematical fact or whether subsequences matches a part of its specification or something else?

main = quickCheckWith stdArgs { maxSize = 25 } prop_subsequenceSize

prop_subsequenceSize :: [Integer] -> Bool
prop_subsequenceSize xs =
  (^) 2 (genericLength xs) == genericLength (subsequences xs
