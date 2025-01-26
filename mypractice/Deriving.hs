module Deriving where

data Choice = Rock | Paper | Scissors
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Eq, Ord, Show, Read)