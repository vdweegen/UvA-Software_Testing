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

{-- Estimated time taken: 6 hours
I tried to create my definitions based on the satisfiability function provided and some logic laws
If satisfiability function is correct then most of the functions will hold. 
For testing, I deciding to create a function that creates a variable length tautologies, contradictions, and propositions
The basic form is the same just the number of props are different. 
Tautologies are disjunctions that have one proposition twice, once negated thus making the form a tautology
Contradictions are conjunctions that have one proposition twice, once negated thus making the form a contradiction
Propositions are just atomic propositions that can be true or false
[TODO] Equiv testing

--}

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




-- exercise2 = map (\(x,y) -> (parse x) == [y] ) expected_forms
exercise2 = do
    print $ all (null) test_incorrect_forms
    print $ and test_expected_forms
    print $ all (not.null) test_correct_forms

{--

    Trying to test this functions forces you to think about the edge cases. What do you expect? What can go wrong? How should it react? And then writing some tests for it. 
    In this case Unit tests. The testing is related to the expected outputs. This function will either return a partial result, get an error or return an empty list when
    it encounters bad input. Partial results are the hardest to test because checking if you have an empty list is not enough. 
    The expected (partial) content should also be checked.

    
    Method: 
    -   Incorrect content
        returns partial form
        Precondition: Input is a String
        Postcondition: output is empty list
    -   Correct content
        The parser should successfully return full form
        Precondition: Input is a String
        Postcondition: Valid form
    -   Expected forms
        The parser should return the expected form after successful parsing
        Precondition: Input is a String
        Postcondition: Output matches the expected form

    
    Possible improvements:
    Testing the expected partials
    Testing the encoding and decoding using the show (Preconditions is that the show works correctly)

--}
test_incorrect_forms =  map (parse) incorrect_forms
test_correct_forms =  map (parse) correct_forms
test_expected_forms = map (\(x,y) ->  head (parse x) == y ) expected_forms

incorrect_forms = [
    "-+(1 0",
    "(1)",
    "(1 0",
    "*1",
    "==>",
    ""]

correct_forms = [
    "-+(1 0)",
    "1",
    "*(1 0)",
    "+(1 10 19)",
    "(1==>2)",
    "*((1==>2) 3)"]

expected_forms = [
    ("(10 ==> 1)", Impl (Prop 10) (Prop 1)),
    ("(10 ==> 9)", Impl (Prop 10) (Prop 9)),
    ("*(10 1)", Cnj [(Prop 10),(Prop 1)]),
    ("-+(1 2)", Neg( Dsj [(Prop 1),(Prop 2)]))
    ]




exercise3 = do
    print ()


{--
Too sleepy to write description will finish in the morning

Need to add distribution when DSJ is on the outside!
--}   

distributeAgain p (Dsj xs) = Dsj (p++xs)
distributeAgain p (Prop xs) = Cnj ((Prop xs):p)

distributeRules xs (Cnj ps) = map (distributeAgain xs) ps
distributeRules xs (Dsj ps) = map (distributeAgain xs) ps
-- distributeRules (Prop p) (Dsj ps) = sortBy sortProps (map (\(Cnj xs) -> Dsj ((Prop p):xs)) ps)
-- distributeRules (Prop p) (Dsj ps) = (map (\(Cnj xs) -> Dsj ((Prop p):xs)) ps)
-- distributeRules (Prop p) (Cnj ps) = Cnj (Prop p : ps)

-- distributeRules (Prop x) (Cnj z) = Cnj [Prop x, Cnj z]

-- distributeRules ps x =  cnf x

distributeDsj fs p =  concatMap (distributeRules fs) p
-- distributeDsj fs ps =  map (\(Dsj x) -> Dsj (x) ) fs
-- distributeDsj f fs =  map (\x -> Dsj [f, x] ) fs

distributeDsjNew [] [] acc = []
distributeDsjNew [] _  acc = acc
distributeDsjNew _ [] acc = acc
-- distributeDsjNew propositions = 

sortProps (Prop x)  _ = LT 
sortProps  _ (Prop x) = GT 
sortProps f1 f2 = GT

filterProps (Prop x) = True
filterProps _ = False

filterCnj (Cnj x) = True
filterCnj _ = False

filterDsj (Dsj x) = True
filterDsj _ = False

-- -- distribute' fs = distributeDsj (map cnf cnj) ps
-- --     where 
-- --         buckets = partition filterProps fs
-- --         ps = map (cnf) $ fst buckets
-- --         cnj = map (cnf) $ snd buckets

-- distribute fs = Dsj dis
--     where 
--         buckets = partition (not.filterCnj) fs
--         ps = map (cnf) $ fst buckets
--         cnj = map (cnf) $ snd buckets
--         dis = foldr (distributeDsj) (filter filterCnj cnj) [ps]
       
test_cnf f = equiv  (nnf $ arrowfree $ head $ parse f) (cnf $ nnf $ arrowfree $ head $ parse f)

distribute fs = Cnj expand'
    where 
        cnjs = (filter filterCnj fs)
        notConj = (filter (not.filterCnj)) fs 
        combineCnj = sequence (map (\(Cnj x) -> x) cnjs)
        -- expand = nub $  concatMap (\(Cnj x) ->  map (\xx -> Dsj (nub (xx:notConj)) ) x) cnjs
        expand' = map (\cnjList ->  Dsj (nub (notConj ++ cnjList))) combineCnj

distribute' fs = Cnj expand
    where 
        cnjs = (filter filterCnj fs)
        notConj = (filter (not.filterCnj)) fs 
        combineCnj = sequence (map (\(Cnj x) -> x) cnjs)
        expand = nub $  concatMap (\(Cnj x) ->  map (\xx -> Dsj (nub (xx:notConj)) ) x) cnjs


uniqueMap f xs = ys
        where
            ys = nub $ map f xs

cnf :: Form -> Form
cnf  = cnf' . nnf . arrowfree 

cnf' :: Form -> Form
cnf' (Prop x) = Prop x
cnf' (Neg (Prop x)) = Neg (Prop x)
cnf' (Cnj fs) = Cnj (map cnf' (sortBy sortProps fs))
cnf' (Dsj fs) 
      | not.null $ filter (filterDsj) fs  = cnf' $ Dsj ((liftDsj fs) ++ (filter (not.filterDsj) fs))
      | not.null $ filter (filterCnj) fs  = cnf' (distribute (uniqueMap cnf' fs))
      | otherwise = Dsj (uniqueMap cnf' fs)
    where
        ffs = sortBy sortProps fs
        liftDsj fs = nub $ concatMap (\(Dsj xs) -> map (\y -> y ) xs)   (filter (filterDsj) fs)
