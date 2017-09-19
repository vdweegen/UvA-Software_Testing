module Exercises where

import Lab3.Lecture3

import Test.QuickCheck
import System.Random


exercise1 = do
  putStr "alwaysTrue is a tautology: "
  print $ tautology alwaysTrue
  putStr "neverTrue is a contradiction: "
  print $ contradiction alwaysFalse
  putStr "alwaysTrue is equivalent to itself: "
  print $ equiv alwaysTrue alwaysTrue
  putStr "neverTrue is also equivalent to itself"
  print $ equiv alwaysFalse alwaysFalse

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

alwaysTrue, alwaysFalse :: Form
alwaysTrue = head $ parse "+(1 -1)"
alwaysFalse = head $ parse "*(1 -1)"

nestedExpression = head $ parse "(+(1 -1) ==> 2)"

allOf :: Eq a => a -> [a] -> Bool
allOf _ [] = True
allOf a (x:xs) = a == x && allOf a xs

-- | Exercise 2 -

-- | Exercise 3 - Convert Formulas into CNF

-- | Exercise 4 - Random form generator
-- | Generate a form => should pick an operator and then form

randomOperator, randomSign, randomLiteral :: IO String
randomOperator = randomFrom operators
randomSign = randomFrom signs
randomLiteral = randomFrom literals

generateForm :: String -> IO String
generateForm "<==>" = do
                        a <- randomLiteral
                        b <- randomLiteral
                        return $ "(" ++ a ++ "<==>" ++ b ++ ")"

randomFrom :: Eq a => [a] -> IO a
randomFrom xs = randomInteger xs >>= (\randIndex -> return (xs !! randIndex))

randomInteger :: Eq a => [a] -> IO Int
randomInteger xs = (randomRIO (0, (length xs)-1))

operators :: [String]
operators = ["+", "*","==>","<=>"]

signs :: [String]
signs = ["", "-"]

literals :: [String]
literals = [ show a | a <- [1..1000]]

-- | Exercise 5 - Bonus Exercise
-- | Time spent: 20 minutes => no useful output, just playing with the Forms to check how to fix it

type Clauses = [Clause]
type Clause = [Int]

alwaysTrueResult, alwaysFalseResult :: Clauses
alwaysTrueResult = [[1, -1]]
alwaysFalseResult = [[1],[-1]]
nestedExpressionResult = [[1, -1], [2]]

exercise5 = do
  putStr "alwaysTrue in CNF: "
  print $ alwaysTrueResult == (cnf2cls alwaysTrue)
  putStr "alwaysFalse in CNF: "
  print $ alwaysFalseResult == (cnf2cls alwaysFalse)

cnf2cls :: Form -> Clauses
cnf2cls form = undefined

extractForms :: Form -> [Form]
extractForms (Cnj fs) = fs
extractForms (Dsj fs) = fs
extractForms (Neg f) = [f]
