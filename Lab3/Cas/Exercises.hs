module Exercises where

import Lab3.Lecture3
import Data.String.Utils
import Data.List

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
    exercise5

-- =============================================================================
-- Exercise 1 :: Time spent: +- 180 min
-- =============================================================================

exercise1 = do
  putStrLn "Contradiction:"
  print $ contradiction form1
  print $ contradiction form2
  print $ contradiction form3
  putStrLn "Tautology:"
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

showIssue = doEntail "*(1 2)" "+(*(1 2) 3)"
showIssue2 = doEntail "*(1 2)" "*(3 4)"

doEntail :: String -> String -> Bool
doEntail f1 f2 = entails (doParse f1) (doParse f2)

doParse :: String -> Form
doParse = head . parse

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
-- Exercise 3 :: Time spent +- 360 min
-- =============================================================================
exercise3 = do
  -- Step #1 :: Remove arrows
  -- Step #2 :: Conversion to negation normal form
  -- Step #3 :: Generate Truth Table
  -- Step #4 :: Every result that is false, negate the literal
  -- Step #5 :: Use the literals to construct the CNF
  -- print $ convertToCNF $ getNonTruths $ nnf $ arrowfree prop
  -- print $ invertLiterals $ getNonTruths $ nnf $ arrowfree prop
  print $ convertToCNF $ invertLiterals $ getNonTruths $ nnf $ arrowfree prop2
  print $ parse $ convertToCNF $ invertLiterals $ getNonTruths $ nnf $ arrowfree prop2

convertToCNF :: [Valuation] -> String
convertToCNF v = andCNF (map ordCNF v)

andCNF :: [String] -> String
andCNF (x:xs)
  | length xs == 0 = x
  | length xs < 2 = "*(" ++ x ++ " " ++ xs !! 0 ++ ")"
  | otherwise = "*(" ++ x ++ " " ++ andCNF xs ++ ")"

ordCNF :: Valuation -> String
ordCNF (x:xs)
  | length xs < 2 && snd x == True && snd (xs !! 0) == True = "+(" ++ show (fst x) ++ " " ++ show (fst (xs !! 0)) ++ ")"
  | length xs < 2 && snd x == False && snd (xs !! 0) == True = "+(-" ++ show (fst x) ++ " " ++ show (fst (xs !! 0)) ++ ")"
  | length xs < 2 && snd x == True && snd (xs !! 0) == False = "+(" ++ show (fst x) ++ " -" ++ show (fst (xs !! 0)) ++ ")"
  | length xs < 2 && snd x == False && snd (xs !! 0) == False = "+(-" ++ show (fst x) ++ " -" ++ show (fst (xs !! 0)) ++ ")"
  | length xs >= 2 && snd x == False = "+(-" ++ show (fst x) ++ " " ++ ordCNF xs ++ ")"
  | otherwise = "+(" ++ show (fst x) ++ " " ++ ordCNF xs ++ ")"

invertLiterals :: [Valuation] -> [Valuation]
invertLiterals v = map invertLiteral v

invertLiteral :: Valuation -> Valuation
invertLiteral v = map revert v

-- Revert Valuations
revert :: (Name,Bool) -> (Name,Bool)
revert (k,v) = if v == True then (k,False) else (k,True)

-- This actually returns all valuations for False
getNonTruths :: Form -> [Valuation]
getNonTruths f = filter (\ v -> not $ evl v f) (allVals f)

-- Define base env
x = Prop 1
y = Prop 2
z = Prop 3

prop = Cnj [(Dsj [x,y]),(Neg z)]
prop2 = Dsj [Cnj [x,y], z]           -- +(*(1 2) 3)
-- prop0 = (Neg (Prop 1))
-- prop1 = (Impl (Prop 1) (Prop 2))
-- prop2 = (Impl (Equiv (Prop 1) (Prop 2)) (Impl (Prop 1) (Prop 3)))

-- =============================================================================
-- Exercise 4
-- =============================================================================

-- =============================================================================
-- Exercise 5
-- =============================================================================
type Clauses = [Clause]
type Clause = [Int]

exercise5 = do
  print $ smashCL $ convertToCl $ show $ convertToCls $ cnf $ nnf $ arrowfree wiki1Input
  print $ smashCL $ convertToCl $ show $ convertToCls $ cnf $ nnf $ arrowfree wiki2Input
  print $ smashCL $ convertToCl $ show $ convertToCls $ cnf $ nnf $ arrowfree wiki3Input

wiki1Input, wiki2Input, wiki3Input :: Form
wiki1Input = doParse "+(-2 -3)"
wiki2Input = doParse "*(+(1 3) +(2 3))"
wiki3Input = doParse "*(1 *(+(2 4) +(2 5)))"

wiki1Result, wiki2Result, wiki3Result :: Clauses
wiki1Result = [[-2, -3]]
wiki2Result = [[1, 3], [2, 3]]
wiki3Result = [[1], [2, 4], [2, 5]]

convertToCl :: String -> String
convertToCl s = replace ")" "]" $ replace "+(" "[" $ replace "*(" "[" $ replace " " "," s

smashCL :: String -> String
smashCL s = if (isInfixOf "[[" s == True) || (isInfixOf "]]" s == True) then do smashCL ( replace "[[" "[" $ (replace "]]" "]" s)) else "[" ++ s ++ "]"

convertToCls :: Form -> Form
convertToCls = cls . nnf . arrowfree

cls :: Form -> Form
cls  = cls' . nnf . arrowfree

cls' :: Form -> Form
cls' (Prop x) = Prop x
cls' (Neg (Prop x)) = Neg (Prop x)
cls' (Cnj fs) = Cnj (map cls' fs)
cls' (Dsj fs)
      | not.null $ filter (filterDsj) fs  = cls' $ Dsj ((liftDsj fs) ++ (filter (not.filterDsj) fs))
      | not.null $ filter (filterCnj) fs  = cls' (distribute (uniqueMap cls' fs))
      | otherwise = Dsj (uniqueMap cls' fs)
    where
        liftDsj fs = nub $ concatMap (\(Dsj xs) -> map (\y -> y ) xs)   (filter (filterDsj) fs)

-- Borrowed the below from our group-deliverable
cnf :: Form -> Form
cnf  = cnf' . nnf . arrowfree

cnf' :: Form -> Form
cnf' (Prop x) = Prop x
cnf' (Neg (Prop x)) = Neg (Prop x)
cnf' (Cnj fs) = Cnj (map cnf' fs)
cnf' (Dsj fs)
  | not.null $ filter (filterDsj) fs  = cnf' $ Dsj ((liftDsj fs) ++ (filter (not.filterDsj) fs))
  | not.null $ filter (filterCnj) fs  = cnf' (distribute (uniqueMap cnf' fs))
  | otherwise = Dsj (uniqueMap cnf' fs)
  where
    liftDsj fs = nub $ concatMap (\(Dsj xs) -> map (\y -> y ) xs)   (filter (filterDsj) fs)

filterDsj :: Form -> Bool
filterDsj (Dsj x) = True
filterDsj _ = False

filterCnj :: Form -> Bool
filterCnj (Cnj x) = True
filterCnj _ = False

filterProps :: Form -> Bool
filterProps (Prop x) = True
filterProps _ = False

distribute :: [Form] -> Form
distribute fs = Cnj expand'
  where
    cnjs = (filter filterCnj fs)
    notConj = (filter (not.filterCnj)) fs
    combineCnj = sequence (map (\(Cnj x) -> x) cnjs)
    expand' = map (\cnjList ->  Dsj (nub (notConj ++ cnjList))) combineCnj

uniqueMap :: Eq a => (a1 -> a) -> [a1] -> [a]
uniqueMap f xs = ys
  where
    ys = nub $ map f xs
