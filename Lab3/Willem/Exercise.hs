module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lab3.Lecture3
import Control.Monad

-- Define Main --
main = do
    putStrLn "===================="
    putStrLn "Assignment 3 / Lab 3"
    putStrLn "===================="
    putStrLn "> Exercise 1"
    exercise1
    putStrLn "> Exercise 2"
    exercise2
    putStrLn "> Exercise 3"
    exercise3
    putStrLn "> Exercise 4"
    exercise4
    putStrLn "> Exercise 5"
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
  print contradictionTest
  putStrLn "tautology:"
  print tautologyTest
  putStrLn "entails:"
  print entailsTest
  putStrLn "equivalence:"
  print equivalenceTest

-- | Exercise 2
-- Spent one hour of thinking about properties, did exercise 4 first, so reuse that generator
-- Maybe add some basic tests?
exercise2 = do
  quickCheck prop_parse_equal
  quickCheck prop_parse_invalid
  quickCheck prop_parse_tail

-- |Checks if the parser returns the same result when parsing the result of the show.
-- Basically testing show and parse at the same time
prop_parse_equal :: Form -> Bool
prop_parse_equal f = (parse $ show f) == [f]

-- | Adding a symbol in front of a valid form will result in a empty result
prop_parse_invalid :: Form -> Bool
prop_parse_invalid f = parse ("*" ++ show f) == []

-- | Adding elements to the tail doesn't change the form
prop_parse_tail :: Form -> Bool
prop_parse_tail f = parse (show f ++ "*") == [f]

-- | Exercise 3 (5 minutes)
-- Taken form the slides, first make sure the formula is arrow free, next transform to NNF
-- Simply look at the example forms to validate
toCNF :: Form -> Form
toCNF = nnf . arrowfree

exercise3 = do
  putStrLn "form1 to CNF:"
  print form1
  print $ toCNF form1
  putStrLn "form2 to CNF:"
  print form2
  print $ toCNF form2
  putStrLn "form3 to CNF:"
  print form3
  print $ toCNF form3

-- | Exercise 4
-- Time spent: 1 hour on arbitrary and generators and another hour on the properties

-- | Generates random formulas, base case is a Prop, we recursively call genForm with a smaller n,
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

-- | No arrows present in the formula after applying toCNF, so no Impl or Equiv.
-- Furthermore check if neg is only applied to prop
isCNF :: Form -> Bool
isCNF f = isProp f || isNeg f || isCnj f || isDsj f

isProp :: Form -> Bool
isProp (Prop _) = True
isProp _ = False

isNeg :: Form -> Bool
isNeg (Neg n) = isProp n
isNeg _ = False

isCnj :: Form -> Bool
isCnj (Cnj cs) = all (\c -> isProp c || isNeg c || isCnj c || isDsj c) cs
isCnj _ = False

isDsj :: Form -> Bool
isDsj (Dsj ds) = all (\d -> isProp d || isNeg d || isCnj d || isDsj d) ds
isDsj _ = False

-- | Helper method te evaluate all sets of values for a formula
evlAll :: Form -> [Bool]
evlAll f = map (`evl` f) (allVals f)

prop_cnf_equiv :: Form -> Bool
prop_cnf_equiv f = equiv (toCNF f) f

prop_cnf_no_arrow :: Form -> Bool
prop_cnf_no_arrow f = isCNF $ toCNF f

prop_cnf_eval_equal :: Form -> Bool
prop_cnf_eval_equal f = evlAll f == evlAll(toCNF f)

exercise4 = do
  quickCheck prop_cnf_equiv
  quickCheck prop_cnf_no_arrow
  quickCheck prop_cnf_eval_equal

-- | Exercise 5
type Clause  = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls = undefined