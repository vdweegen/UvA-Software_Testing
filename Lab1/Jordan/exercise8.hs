import Test.QuickCheck
import Data.List
import Prelude

-- Not done/started only added some scaffolding

data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)
 
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Peter Mathew = True
accuses Peter Jack = True

main = do 
    print boys
    print $ accuses Matthew Peter