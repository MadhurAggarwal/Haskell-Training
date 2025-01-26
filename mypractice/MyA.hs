module MyA where
import Prelude hiding (length, null, sum, take, drop, map, filter, foldr)

data Choice = Rock | Paper | Scissors
    deriving Show

isPaper :: Choice -> Bool
isPaper Paper = True
isPaper Rock = False
isPaper Scissors = False

null :: [a] -> Bool
null [] = True
null (_:_) = False

-- null2 :: Eq a => [a] -> Bool
-- null2 xs = xs == []
-- Requires list to be of Eq elements, so adds Overloading requirements

-- null3 :: [a] -> Bool
-- null3 xs = length xs == 0
-- recursive, calculates entire list length, doesnt work on infinite lists [1..]

length :: [a] -> Int
length [] = 0
length (_:x) = 1+ (length x)

noPaper :: [Choice] -> Bool
noPaper [] = True
noPaper (a:b) = (not(isPaper a)) && (noPaper b)

sum :: [Int]->Int 
sum [] = 0
sum (a:b) = a + (sum b)

data List a = Nil | Cons a (List a)
  deriving Show

from :: List a -> [a]
from Nil = []
from (Cons a b) = a:(from b)

to :: [a] -> List a
to [] = Nil
to (a:b) = Cons a (to b)

evens :: [Int] -> [Int]
evens [] = []
evens (a:b) = if (even a) then (a:(evens b)) else (evens b)

sumEvenSquares :: [Int] -> Int
sumEvenSquares [] = 0
sumEvenSquares (a:b) = if (even a) then ((a*a)+(sumEvenSquares b)) else (sumEvenSquares b)

allEven :: [Int] -> Bool
allEven [] = True
allEven (a:b) = (even a) && (allEven b)

isAscending :: [Int] -> Bool
isAscending [] = True
isAscending [a] = True
isAscending (a:(b:c)) = (a<b) && (isAscending (b:c))

data Nat = Zero | Suc Nat
  deriving Show

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (Suc a) = 1+(fromNat a)

add :: Nat -> Nat -> Nat
add Zero a = a
add (Suc x) a = Suc (add x a)