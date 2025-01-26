{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFoldable #-}
module Datatypes where

-- import Prelude hiding ()

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Eq)

oneTree :: Tree Int 
oneTree = Node (Leaf 1) (Leaf 2)
 
binaryTree :: Tree Int
binaryTree = 
    Node 
        (Node (Leaf 4) (Leaf 5)) 
        (Leaf 6)

data Choice = Rock | Paper | Scissors
    deriving Show

data MYData a = One | Three | Two a
    deriving (Show, Foldable)

data User = MakeUser {uid :: Int, uname :: String, uemail :: String}
    deriving Show

newtype UserId = MakeUserId Int
    deriving Show 

uid1 :: UserId 
uid2 :: UserId
uid1 = MakeUserId 42
uid2 = MakeUserId 37

newtype Email = MakeEmail {getEmail :: String}
    deriving Show

-- error, newtype should have 1 constructor
-- newtype Madhur = StringId String | IntId Int

type IntList = [Int]
type UserName = String

