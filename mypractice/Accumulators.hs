{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module Accumulators where 

import Prelude hiding (reverse,sum,elem, map)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

helper :: [a] -> [a] -> [a]
helper xs [] = xs
helper xs (y:ys) = helper (y:xs) ys

reverse' :: [a] -> [a]
reverse' = helper []

sumAux :: Num a => a -> [a] -> a
sumAux !acc [] = acc
sumAux !acc (x:xs) = sumAux (acc+x) xs 

sum :: Num a => [a] -> a 
sum = sumAux 0


elemAux :: Eq a => Bool -> a -> [a] -> Bool
elemAux !acc _ [] = acc
elemAux !acc a (x:xs) = elemAux (acc || a==x) a xs

elem' :: Eq a => a -> [a] -> Bool
elem' = elemAux False

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem a (x:xs) = a==x || elem a xs

map :: (a->b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- map :: (a->b) -> [a] -> [b]
-- map f = reverse . go []
--     where 
--         -- go :: [b] -> [a] -> [b]
--         go !acc []     = acc 
--         go !acc (x:xs) = go ((f x) : acc) xs