module Exercises where

import Lab3.Lecture3

-- Define Main --
main = do
    putStrLn $ "===================="
    putStrLn $ "Assignment 3 / Lab 3"
    putStrLn $ "===================="
    putStrLn $ "> Exercise 1"
    exercise1
    putStrLn $ "> Exercise 2"
    -- exercise2
    putStrLn $ "> Exercise 3"
    -- exercise3
    putStrLn $ "> Exercise 4"
    -- exercise4
    putStrLn $ "> Exercise 5"
    -- exercise5

-- =============================================================================
-- Exercise 1 :: Time spent: 180 minutes
-- =============================================================================

exercise1 = do
  putStrLn "Contradiction:"
  print $ contradiction form1
  print $ contradiction form2
  print $ contradiction form3
  putStrLn "tautology:"
  print $ tautology form1
  print $ tautology form2
  print $ tautology form3
  putStrLn "Entails:"
  print $ entails form1 form1
  print $ entails form1 form2
  print $ entails form1 form3
  print $ entails form2 form2
  print $ entails form2 form1
  print $ entails form2 form3
  print $ entails form3 form3
  print $ entails form3 form1
  print $ entails form3 form2
  putStrLn "Equivalance:"
  print $ equiv form1 form1
  print $ equiv form1 form2
  print $ equiv form1 form3
  print $ equiv form2 form2
  print $ equiv form2 form1
  print $ equiv form2 form3
  print $ equiv form3 form3
  print $ equiv form3 form1
  print $ equiv form3 form2

-- contradiction :: check
contradiction :: Form -> Bool
contradiction f = all (\ v -> not $ evl v f) (allVals f)

-- tautology :: check
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- logical entailment :: check
entails :: Form -> Form -> Bool
entails x y = entailment (allResults x) (allResults y)

entailment :: [Bool] -> [Bool] -> Bool
entailment [] [] = True
entailment [] (y:ys) = entailment [True] ys
entailment (x:xs) [] = False
entailment (x:xs) (y:ys)
  | x && not y = False
  | otherwise = entailment xs ys

allResults :: Form -> [Bool]
allResults x = map (flip evl x) (allVals x)


-- logical equivalence :: check
equiv :: Form -> Form -> Bool
equiv x y = equivalent (allResults x) (allResults y)

equivalent :: [Bool] -> [Bool] -> Bool
equivalent [] [] = True
equivalent [] (y:ys) = False
equivalent (x:xs) [] = False
equivalent (x:xs) (y:ys)
  | x /= y = False
  | otherwise = equivalent xs ys

-- =============================================================================
-- Exercise 2
-- =============================================================================

-- =============================================================================
-- Exercise 3
-- =============================================================================

-- =============================================================================
-- Exercise 4
-- =============================================================================

-- =============================================================================
-- Exercise 5
-- =============================================================================
