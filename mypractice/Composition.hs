{-# OPTIONS_GHC -Wall #-}
module Composition where 

import Prelude hiding ((.), id, ($))

sumEvenSquares :: [Int] -> Int 
-- sumEvenSquares xs = sum (map (\ x -> x * x) (filter even xs))
sumEvenSquares xs = sum $ map (\ x -> x * x) $ filter even xs 
-- sumEvenSquares xs = sum (map (^ (2 :: Int)) (filter even xs))

sumEvenSquares' :: [Int] -> Int 
sumEvenSquares' = sum . map (\ x -> x * x) . filter even 

-- Function Compostion
(.) :: (a -> b) -> (c -> a) -> c -> b
f . g = \ x -> f (g x)

-- (.) f g x = f (g x)
-- (f . g) x = f (g x)

countLongerThanFive :: [[a]] -> Int 
countLongerThanFive = length . filter ((>= 5) . length)

id :: a -> a
id x = x 

-- map id xs = xs 

and :: [Bool] -> Bool 
and = all id

listOfFunctions :: [Int -> Int]
listOfFunctions = [(+1), (*2), (+7), (*4)]

($) :: (a -> b) -> a -> b
($) = id
infixr 0 $

-- f $ x = f x