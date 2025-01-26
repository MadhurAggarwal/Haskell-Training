{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}
module FunctorFoldable where

import Prelude hiding ()

mapList :: (a->b) -> [a] -> [b]
mapList _ []     = []
mapList f (x:xs) = f x : mapList f xs

mapMaybe :: (a->b) -> Maybe a -> Maybe b
mapMaybe _ Nothing  = Nothing
mapMaybe f (Just x) = Just (f x)

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show)

mapTree :: (a->b) -> Tree a -> Tree b
mapTree f (Leaf a)     = Leaf (f a)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

class MyFunctor f where 
    myfmap :: (a->b) -> f a -> f b 

instance MyFunctor [] where
    myfmap :: (a->b) -> [a] -> [b]
    -- myfmap :: (a->b) -> [] a -> [] b
    myfmap _ [] = []
    myfmap f (x:xs) = f x : fmap f xs

instance MyFunctor Maybe where 
    myfmap :: (a->b) -> Maybe a -> Maybe b 
    myfmap _ Nothing  = Nothing 
    myfmap f (Just a) = Just (f a)

instance Functor Tree where 
    fmap :: (a->b) -> Tree a -> Tree b 
    fmap f (Leaf a)   = Leaf (f a)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- data Either a b = Left a | Right b 

instance MyFunctor (Either x) where
    myfmap :: (a->b) -> Either x a -> Either x b
    myfmap _ (Left x)  = Left x
    myfmap f (Right a) = Right (f a)

toList :: Tree a -> [a]
toList (Leaf x)   = [x]
toList (Node l r) = toList l ++ toList r

instance Foldable Tree where 
    foldr :: (a -> r -> r) -> r -> Tree a -> r
    -- foldr cons basecase (Leaf a)   = cons a basecase
    -- foldr cons basecase (Node l r) = cons _ (foldr cons basecase _)
    foldr cons nil t = foldr cons nil (toList t)