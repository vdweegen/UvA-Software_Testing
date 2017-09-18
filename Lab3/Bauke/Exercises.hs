module Exercises where

import Lab3.Lecture3

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

alwaysSatisfies, neverSatisfies :: Form
alwaysSatisfies = head $ parse "+(1 -1)"
neverSatisfies = head $ parse "*(1 -1)"

allOf :: Eq a => a -> [a] -> Bool
allOf _ [] = True
allOf a (x:xs) = a == x && allOf a xs


