{-# OPTIONS_GHC -Wall #-}
module Pair where

-- import Data.Maybe
-- import Control.Applicative
import Prelude hiding (fst, uncurry, zip, zipWith, lookup)

data Pair a b = MakePair a b
    deriving Show

-- fst :: Pair a b -> a
-- fst (MakePair x _) = x

fst :: (a,b) -> a
fst (x,_) = x

-- plus :: (Int, Int) -> Int 
-- plus (a,b) = a+b 

uncurry :: (a->b->c) -> ((a,b)->c)
-- uncurry :: (a->b->c) -> (a,b) -> c
uncurry f (x,y) = f x y

-- plus :: (Int, Int) -> Int
-- plus (a,b) = uncurry (+) (a,b)

plus :: (Int,Int) -> Int
plus = uncurry (+)

-- zip :: [a] -> [b] -> [(a,b)]
-- zip [] _ = []
-- zip _ [] = []
-- zip (x:xs) (y:ys) = (x,y) : zip xs ys

zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

zip :: [a] -> [b] -> [(a,b)]
zip = zipWith (,)

example :: [(Int,String)]
example = 
    [
        (1,"Frodo"),
        (2,"Bilbo"),
        (3,"Gandalf"),
        (11,"Yoda"),
        (40,"Gollum"),
        (11,"Sauron")
    ]

-- lookup :: Eq key => key -> [(key,val)] -> val
-- lookup _ [] = error "key not found"
-- lookup key ((a,b) : table) = if key ==  a then b else lookup key table

data LookupResult val = LookupFailed | LookupSuccess val 
    deriving Show 

-- lookup :: Eq key => key -> [(key,val)] -> LookupResult val
-- lookup _ [] = LookupFailed
-- lookup key ((a,b) : table) = if key == a then LookupSuccess b else lookup key table

lookup :: Eq key => key -> [(key,val)] -> Maybe val
lookup _ [] = Nothing
lookup key ((a,b) : table) = if key == a then Just b else lookup key table

fromMaybe :: a -> Maybe a -> a
fromMaybe defaultValue Nothing = defaultValue
fromMaybe _ (Just x) = x

mapMaybe :: (a->b) -> Maybe a -> Maybe b 
mapMaybe _ Nothing = Nothing  
mapMaybe f (Just a) = Just (f a) 

orelse :: Maybe a -> Maybe a -> Maybe a 
orelse Nothing y = y
orelse (Just x) _ = (Just x)

-- this is similar to OR 
-- (||) :: Bool -> Bool -> Bool 
-- (||) False y = y
-- (||) True _ = True

secondTable :: [(Int, String)]
secondTable = 
    [
        (4,"Minions")
    ]

thirdTable :: [(Int,String)]
thirdTable = undefined