{-# LANGUAGE InstanceSigs #-}
module Overloading where 

import Prelude hiding (Eq(..), Ord(..))
import Text.Read

class Eq a where
    (==) :: a -> a -> Bool 
    x == y = not (x /= y)
    (/=) :: a -> a -> Bool
    x /= y = not (x == y)
    {-# MINIMAL (==) | (/=) #-}

instance Eq Bool where
    -- (==) :: Bool -> Bool -> Bool
    True  == True  = True 
    False == False = True
    _     == _     = False

data Choice = Rock | Paper | Scissors 

instance Eq Choice where
    -- (==) :: Choice -> Choice -> Bool
    Rock     == Rock     = True
    Paper    == Paper    = True
    Scissors == Scissors = True
    _        == _        = False

instance Eq a => Eq [a] where 
    (==) :: [a] -> [a] -> Bool
    []     == []     = True 
    (x:xs) == (y:ys) = (x==y) && (xs == ys)
    _      == _      = False


class Eq a => Ord a where
    (<=) :: a -> a -> Bool 
    (<)  :: a -> a -> Bool 
    x < y = x <= y && not (x == y)

instance Ord Bool where
    False <= y = True 
    True  <= y = y

-- instance Eq a => Eq (Maybe a) where 

-- instance Ord a => Ord (Maybe a) where

-- using the defult Show class
instance Show Choice where
    show :: Choice -> String 
    show Rock     = "Rock"
    show Paper    = "Paper"
    show Scissors = "Scissors"