-- Implementation time: 60 minutes
-- Reason: First part was fairly easy. The last part, on how to find the solution was hard.
-- had to peek in the group members solutions' to find the correct implementation.


data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq, Show)

main = print $ "honest boys: " ++ (show honest) ++ ", guilty boy: " ++ (show $ head $ guilty)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew someBoy | someBoy == Matthew = False
                        | someBoy == Carl = False
                        | otherwise = True

accuses Peter someBoy | someBoy == Matthew = True
                      | someBoy == Jack = True
                      | otherwise = False

accuses Jack someBoy | True == (accuses Matthew someBoy) = False
                     | True == (accuses Peter someBoy) = False
                     | otherwise = True

accuses Arnold someBoy = ((True == (accuses Matthew someBoy)) || (True == (accuses Peter someBoy)))
                        && (False == (True == (accuses Matthew someBoy)) && (True == (accuses Peter someBoy)))

accuses Carl someBoy = False == (accuses Arnold someBoy)

accusers :: Boy -> [Boy]
accusers thisBoy = [ boy | boy <- boys, accuses boy thisBoy ]

guilty :: [Boy]
guilty = [ guiltyBoy | guiltyBoy <- boys, 3 == (length $ accusers guiltyBoy)]

honest :: [Boy]
honest = [ honestBoy | honestBoy <- boys, accuses honestBoy (head guilty)]
