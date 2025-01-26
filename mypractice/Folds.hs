module Folds where 

import Prelude hiding (map, elem, and, reverse, sum, length, foldr)
-- Data.List.foldl'
import Data.List hiding (foldr, and, map, elem) 

-- Standard Design Pattern
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem a (x:xs) = a==x || elem a xs

map :: (a->b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

-- General Function (Foldr)
foldr :: (a->r->r) -> r -> [a] -> r
foldr cons baseresult [] = baseresult
foldr cons baseresult (x:xs) = cons x (foldr cons baseresult xs)

elem' :: Eq a => a -> [a] -> Bool 
elem' a = foldr (\ x r -> a==x || r) False

map' :: (a->b) -> [a] -> [b]
map' f = foldr (\ x r -> f x : r) []

and' :: [Bool] -> Bool
and' = foldr (&&) True

-- General design pattern for accumulator design
reverse :: [a] -> [a]
reverse = go []
    where
        go !acc []     = acc
        go !acc (x:xs) = go (x:acc) xs

sum :: Num a => [a] -> a 
sum = go 0
    where 
        go !acc []     = acc
        go !acc (x:xs) = go (acc+x) xs 

length :: [a] -> Int 
length = go 0
    where
        go !acc []     = acc
        go !acc (_:xs) = go (acc+1) xs

-- General Func for accumulator pattern (Foldl')
-- if we change updateAcc to (updateAcc x acc) then the type of foldr and foldl' is same
-- foldl' :: (r -> a -> r) -> r -> [a] -> r
-- foldl' updateAcc initalAcc = go initalAcc 
--     where
--         go !acc []     = acc 
--         go !acc (x:xs) = go (updateAcc acc x) xs

reverse' :: [a] -> [a]
reverse' = foldl' (\ acc x -> x : acc ) []

sum' :: Num a => [a] -> a 
sum' = foldl' (\ acc x -> acc + x) 0

length' :: [a] -> Int 
length' = foldl' (\ acc _ -> acc + 1) 0