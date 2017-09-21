module Lab3 where
import Data.List
import System.Random
import Test.QuickCheck
import Lab3.Lecture3


-- http://intrologic.stanford.edu/notes/chapter_03.html
-- http://logictools.org/



contradiction :: Form -> Bool
contradiction  = not.satisfiable

-- Negation of contradiction (Need more info)
tautology :: Form -> Bool
tautology f = contradiction (Neg f)

--Unsatisfiability Theorem
entailment' :: Form -> Form -> Bool
entailment' f phi = contradiction (Cnj [f, (Neg phi)])

-- Deduction Theorem ( Need more info )
entailment :: Form -> Form -> Bool
entailment f phi = tautology (Impl f phi)

--The Equivalence Theorem
equiv :: Form -> Form -> Bool
equiv f phi = (entailment f phi) && (entailment phi f)
 
showTables f1 f2 =  map (\x -> map (\ v -> evl v x) (allVals x)) [f1, f2]

satisfiable' f = map (\ v -> evl v f) (allVals f)

form4 = Cnj [p, q]
form5 = (Equiv p (Neg p))

generateProps :: Int -> [Form]
generateProps n =  map (Prop) $ [1..n]

generateContradictions :: [Name] -> Form
generateContradictions xs =  Cnj $ (Neg (Prop (last xs))) : map (Prop) xs

generateTautology :: [Name] -> Form
generateTautology xs =  Dsj $ (Neg (Prop (last xs))) : map (Prop) xs

generateTautology2 :: [Name] -> Form
generateTautology2 xs =  Equiv f f
        where 
            f = Dsj $ (Neg (Prop (last xs))) : map (Prop) xs



test_entailment_incorrect n = all (uncurry entailment) $ zip p phi
        where 
            phi =  [ generateTautology2 [1..x] | x <- [1..n]]
            p = generateProps n

test_entailment_correct n = all (uncurry entailment) $ zip phi phi
        where 
            phi =  [ generateTautology2 [1..x] | x <- [1..n]]

test_entailment_correct2 n = all (uncurry entailment) $ zip phi p
            where 
                phi =  [ generateContradictions [1..x] | x <- [1..n]]
                p = generateProps n

test_contradictions_correct n = all contradiction $ [ generateContradictions [1..x] | x <- [1..n]]
test_contradictions_incorrect n = not.all (contradiction) $ [ generateTautology [1..x] | x <- [1..n]] 
test_contradictions_incorrect2 n = not.all (contradiction) $ [ generateTautology2 [1..x] | x <- [1..n]] 

test_tautology_correct n = all tautology $ [ generateTautology [1..x] | x <- [1..n]]
test_tautology_correct2 n = all tautology $ [ generateTautology2 [1..x] | x <- [1..n]] 
test_tautology_incorrect1 n = not.all (tautology) $ [ generateContradictions [1..x] | x <- [1..n]] 



-- print $ map (contradiction) $ [ generateContradictions [1..x] | x <- [1..10]]

--main = print $ equi [True, True, False, True] [True, True, False, True]

-- Estimated time taken: 6 hours
-- I tried to create my definitions based on the satisfiability function provided and some logic laws
-- If satisfiability function is correct then most of the functions will hold. 
-- For testing, I deciding to create a function that creates a variable length tautologies, contradictions, and propositions
-- The basic form is the same just the number of props are different. 
-- Tautologies are disjunctions that have one proposition twice, once negated thus making the form a tautology
-- Contradictions are conjunctions that have one proposition twice, once negated thus making the form a contradiction
-- Propositions are just atomic propositions that can be true or false
-- [TODO] Equiv testing

exercise1 =  do 
    putStrLn "Test Contradictions: List of contradictions"
    print $ test_contradictions_correct 10
    putStrLn "Test Contradictions: List of tautologies Conjuctions"
    print $ test_contradictions_incorrect 10
    putStrLn "Test Contradictions: List of tautologies Self Equiv"
    print $ test_contradictions_incorrect2 10
    putStrLn "Test Tautologies: List of tautologies Conjuctions"
    print $ test_tautology_correct 10
    putStrLn "Test Tautologies: List of tautologies Self Equiv"
    print $ test_tautology_correct2 10
    putStrLn "Test Tautologies: List of contradictions"
    print $ test_tautology_incorrect1 10
    putStrLn "Test Entailments: List of tautologies entailing themselves"
    print $ test_entailment_correct 10
    putStrLn "Test Entailments: List of contradictions that entail everything"
    print $ test_entailment_correct2 10    
    putStrLn "Test Entailments: List of non tautologies that cannot entail tautologies"
    print $ test_entailment_incorrect 10
