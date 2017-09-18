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
    exercise2
    putStrLn $ "> Exercise 3"
    exercise3
    putStrLn $ "> Exercise 4"
    -- exercise4
    putStrLn $ "> Exercise 5"
    -- exercise5

-- =============================================================================
-- Exercise 1 :: Time spent: +- 180 min
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
-- Exercise 2 :: Time spent +- 150 min
-- =============================================================================
exercise2 = do
  -- test stuff
  print $ parse "*(1 + (2 -3))"
  print $ parse "+(1 + (2 -3))"
  print $ parse "*(1 + (-2 -3))"
  -- Check all tests
  print $ and $ map (uncurry propositional_test) tests

propositional_test :: [Char] -> [Form] -> Bool
propositional_test s f = parse s == f

tests =
  [
  -- empty
  ("", []),
  -- single value
  ("1", [(Prop 1)]),
  -- parenthesis (empty)
  ("(1)", []),
  -- conjunction (empty)
  ("*", []),
  -- disjunction (empty)
  ("+", []),
  -- negation (empty)
  ("-", []),
  -- implication (empty)
  ("==>", []),
  -- conjunction
  ("*(1 2)", [Cnj [Prop 1, Prop 2]]),
  -- disjunction
  ("+(1 2)", [Dsj [Prop 1, Prop 2]]),
  -- negation
  ("-1", [(Neg (Prop 1))]),
  -- implication
  ("(1==>2)", [Impl (Prop 1) (Prop 2)]),
  -- equivalence
  ("(1<=>1)", [Equiv (Prop 1) (Prop 1)]),
  -- complex tests
  ("((1==>2) ==> (1==>3))", [Impl (Impl (Prop 1) (Prop 2)) (Impl (Prop 1) (Prop 3))]),
  ("(-(1==>2) ==> (1==>3))", [Impl (Neg (Impl (Prop 1) (Prop 2))) (Impl (Prop 1) (Prop 3))]),
  ("(-(1==>2) <=> (1==>3))", [Equiv (Neg (Impl (Prop 1) (Prop 2))) (Impl (Prop 1) (Prop 3))]),
  ("((1<=>2) ==> (1==>3))", [Impl (Equiv (Prop 1) (Prop 2)) (Impl (Prop 1) (Prop 3))])
  ]

-- =============================================================================
-- Exercise 3
-- =============================================================================
exercise3 = do
  -- Use the following 'given' stuff: Imp Equiv Neg Prop Dsj Cnj

  -- Step #1 :: Remove arrows
  -- Step #2 :: Conversion to negation normal form
  -- Step #3 :: Remove Conjunctions
  -- Step #4 :: Remove Disjunctions
  print $ nnf $ convertToCNF prop2

convertToCNF :: Form -> Form
convertToCNF f = arrowfree f

prop0 = (Neg (Prop 1))
prop1 = (Impl (Prop 1) (Prop 2))
prop2 = (Impl (Equiv (Prop 1) (Prop 2)) (Impl (Prop 1) (Prop 3)))

-- =============================================================================
-- Exercise 4
-- =============================================================================

-- =============================================================================
-- Exercise 5
-- =============================================================================
