module Basics where
import Prelude hiding (not, (||),elem)

ten = five+five
five = 2+3
aList = [1,2,3,4,five]
six = 3+3
double = \x -> x+x

distance :: Num a => a->a->a
distance x y = abs(x-y)

triple = \x -> x+x+x

data MyChoice = Rock | Paper | Scissors
    deriving Show

improve :: MyChoice -> MyChoice
improve Rock = Paper
improve Paper = Scissors
improve Scissors = Rock

worsen :: MyChoice -> MyChoice
worsen Rock = Scissors
worsen Paper = Rock
worsen Scissors = Paper

not :: Bool -> Bool
not False = True
not True = False

(||) :: Bool->Bool->Bool
True  || y = True
False || y = y
infixr 2 ||
-- we can use any format for definition
-- x || y = 
-- (||) x y =  

data MyList a = MyNil | MyCons a (MyList a)
    deriving Show

firstList :: MyList Int
firstList = MyCons 1 (MyCons 2 (MyCons 3 MyNil))

-- elem x [] = ...
-- Finding Element in Empty List gives False
elem1 xToFind MyNil = False
-- Finding Element in rest of the List
elem1 xToFind (MyCons a aList) = (xToFind == a) || (elem1 xToFind aList)

elem2 x [] = False
elem2 x (y : li) = (x==y) || (elem2 x li)