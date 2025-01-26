{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude hiding (length, product, map, filter, any, drop, (++), reverse)

length :: [a] -> Int
-- length = undefined
-- length = _
-- length xs = _ 
length [] = 0
length (_:xs) = 1+length xs

product :: Num p => [p] -> p 
product [] = 1
product (x:xs) = x * product xs

map :: (a->b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- filter :: (a->Bool) -> [a] -> [a]
-- filter _ [] = []
-- filter f (x:xs) = if f x then x : filter f xs else filter f xs 

-- filter :: (a->Bool) -> [a] -> [a]
-- filter _ [] = []
-- filter f (x:xs) = 
--     case f x of 
--         True -> x : filter f xs
--         False -> filter f xs

filter :: (a->Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs) 
    | f x = x : filter f xs
    | otherwise = filter f xs

any :: (a->Bool) -> [a] -> Bool
any _ [] = False
any f (x:xs) = if f x then True else any f xs

drop :: Int -> [a] -> [a]
drop _ [] = []
drop num (x:xs) = if num>0 then drop (num-1) xs else x:xs

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

(++) :: [a] -> [a] -> [a]
(++) = append
infixr 5 ++

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]