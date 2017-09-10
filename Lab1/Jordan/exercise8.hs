import Test.QuickCheck
import Data.List
import Prelude

-- Not done/started only added some scaffolding

data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)
 
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses x y = False

main = do 
    print boys
    print $ accuses Matthew Peter