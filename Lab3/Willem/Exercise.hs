module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lab3.Lecture3
import Control.Monad

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

-- | Exercise 1
-- Time spent: 2 hours
-- We use the forms from Lecture3.hs to check some basic properties of the functions

-- | logical contradiction
-- No set of values exists where the form returns true, so if we cannot find a
-- satisfiable set of values the form is a contradiction
contradiction :: Form -> Bool
contradiction f = not $ satisfiable f

-- | logical tautology
-- Instead of finding one set of values which make the form return true like in satisfiable,
-- it should return true for all sets of values
tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

-- | logical entailment
-- When the first form returns true, and the second form returns true, we return true,
-- this is an implication which we should check for every set of values, thus a tautology
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- | logical equivalence
-- Each set of values should return the same value for both forms,
-- which means we can define an equivalence relation,
-- and this should return true for every set of values, so it should be a tautology
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

-- Test contradiction
contradictionTest, tautologyTest, entailsTest, equivalenceTest :: Bool
contradictionTest =  (contradiction form1 == False)
                  && (contradiction form2 == False)
                  && (contradiction form3 == False)

tautologyTest =  (tautology form1 == True)
              && (tautology form2 == False)
              && (tautology form3 == True)

entailsTest =  (entails form1 form1 == True)
            && (entails form1 form2 == False)
            && (entails form1 form3 == True)
            && (entails form2 form1 == True)
            && (entails form2 form2 == True)
            && (entails form2 form3 == True)
            && (entails form3 form1 == True)
            && (entails form3 form2 == False)
            && (entails form3 form3 == True)

equivalenceTest =  (equiv form1 form1 == True)
                && (equiv form1 form2 == False)
                && (equiv form1 form3 == True)
                && (equiv form2 form1 == False)
                && (equiv form2 form2 == True)
                && (equiv form2 form3 == False)
                && (equiv form3 form1 == True)
                && (equiv form3 form2 == False)
                && (equiv form3 form3 == True)

exercise1 = do
  putStrLn "contradictionTest:"
  print $ contradictionTest
  putStrLn "tautology:"
  print $ tautologyTest
  putStrLn "entails:"
  print $ entailsTest
  putStrLn "equivalence:"
  print $ equivalenceTest

-- | Exercise 2

-- | Exercise 3

-- | Exercise 4

-- | genForm (spent 1 hour on arbitrary and generators)
-- Generates random formulas, base case is a Prop, we recursively call genForm with a smaller n,
-- We could use a different function to shrink n or use a different value for the multipleNextForm
-- which doesn't depend on n, however the mod takes care of that now
genForm :: Int -> Gen Form
genForm n | n == 0 = genProp
          | n > 0  = oneof [
            genProp,
            liftM Neg nextForm,
            liftM Cnj multipleNextForm,
            liftM Dsj multipleNextForm,
            liftM2 Impl nextForm nextForm,
            liftM2 Equiv nextForm nextForm
          ] where nextForm = genForm (n `div` 2)
                  multipleNextForm = vectorOf (n `mod` 10) nextForm
                  genProp = Prop `liftM` choose(0,9)

instance Arbitrary Form where
  arbitrary = sized genForm

-- | Exercise 5
type Clause  = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls = undefined