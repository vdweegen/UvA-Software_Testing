module Exercises where

import Lab3.Lecture3


exercise1 = do
  putStr "alwaysTrue is a tautology: "
  print $ tautology alwaysTrue
  putStr "neverTrue is a contradiction: "
  print $ contradiction neverTrue
  putStr "alwaysTrue is equivalent to itself: "
  print $ equiv alwaysTrue alwaysTrue
  putStr "neverTrue is also equivalent to itself"
  print $ equiv neverTrue neverTrue

-- | Exercise 1, Give the definitions for the predicates
-- | Time spent: 30 minutes
-- | Most simple thing what works is checking the definitions against predicates which are always true or always false.
-- | Examples: A OR not A => always true. A AND not A => never true

-- | There is no set of values for which the form returns true
contradiction :: Form -> Bool
contradiction form = not $ satisfiable form

-- | There is no set of values for which the form returns false
tautology :: Form -> Bool
-- tautology form = allOf True (map (flip evl form) (allVals form))
tautology f = all (\v -> evl v f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails form1 form2 = isEntailment (allResults form1, allResults form2)

isEntailment :: ([Bool],[Bool]) -> Bool
isEntailment ([],[]) = True
isEntailment ((x:xs),(y:ys)) | x && (not y) = False
                             | otherwise = isEntailment (xs,ys)
-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv form1 form2 = allResults form1 == allResults form2

allResults :: Form -> [Bool]
allResults form = map (flip evl form) (allVals form)

alwaysTrue, neverTrue :: Form
alwaysTrue = head $ parse "+(1 -1)"
neverTrue = head $ parse "*(1 -1)"

allOf :: Eq a => a -> [a] -> Bool
allOf _ [] = True
allOf a (x:xs) = a == x && allOf a xs

-- | Exercise 2 -

-- | Exercise 3 - Convert Formulas into CNF
