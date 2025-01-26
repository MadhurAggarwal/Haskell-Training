{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}

module MyC where

import Data.Char (isSpace, toUpper)
import Data.List (foldl')
import Prelude hiding (concat, product, take)

-- 1
product :: Num a => [a] -> a
product = go 1
    where 
        go !acc []     = acc
        go !acc (x:xs) = go (acc * x) xs

-- 2
product' :: Num a => [a] -> a
product' = foldl' (*) 1

-- 3
countWhiteSpaceStrings :: [String] -> Int
countWhiteSpaceStrings = length . filter (all isSpace)

-- 4
removeLongWords :: Int -> String -> String
removeLongWords n = unwords . filter ((<= n) . length) . words 

-- 5
concat :: [[a]] -> [a]
concat = foldr (++) []

-- 6
data Dir = Up | Rgt | Down | Lft
  deriving (Show)

data Pos = MkPos {pUp :: !Int, pRight :: !Int}
  deriving (Show)

origin :: Pos
origin = MkPos 0 0

goDir :: Pos -> Dir -> Pos
goDir pos Up   = pos {pUp    = pUp pos + 1}
goDir pos Down = pos {pUp    = pUp pos - 1}
goDir pos Rgt  = pos {pRight = pRight pos + 1}
goDir pos Lft  = pos {pRight = pRight pos - 1}

goDirs :: Pos -> [Dir] -> Pos
goDirs = foldl' goDir

-- 7
-- foldr
inits :: [a] -> [[a]]
inits =  foldr (\ x res -> [] : (map (x:) res)) [[]]

-- foldl'
inits' :: [a] -> [[a]]
inits' xs = (fst (initsAux xs))
    where
        initsAux :: [a] -> ([[a]], [a])
        initsAux = foldl' (\ (res, acc) x -> (res ++ [acc ++ [x]], acc ++ [x])) ([[]],[])

-- 8
data BinTree a
  = Bin (BinTree a) a (BinTree a)
  | Empty
  deriving (Eq, Show)

tree1 :: BinTree Char
tree1 = Bin Empty 'x' Empty

tree2 :: BinTree Char
tree2 = Bin Empty 'y' Empty

tree3 :: BinTree Char
tree3 = Bin tree1 'a' tree2

tree4 :: BinTree Char
tree4 = Bin tree3 'b' tree1

tree5 :: BinTree Char
tree5 = Bin Empty 'c' tree4

tree6 :: BinTree Char
tree6 = Bin tree5 'd' tree4

mapBinTree :: (a -> b) -> BinTree a -> BinTree b 
mapBinTree _ Empty = Empty
mapBinTree f (Bin x a y) = Bin (mapBinTree f x) (f a) (mapBinTree f y)

-- 9
labelTree :: BinTree a -> BinTree (a, Int)
labelTree a =  fst (labelAux 1 a)

labelAux :: Int -> BinTree a -> (BinTree (a, Int), Int)
labelAux x Empty       = (Empty, x)
labelAux x (Bin l a r) = (Bin lRes (a, xl) rRes, xr)
    where
        (lRes, xl) = labelAux x l
        (rRes, xr) = labelAux (xl + 1) r

-- 10
take :: Int -> [a] -> [a]
take _ [] = []
take n (x : xs)
  | n > 0 = x : take (n - 1) xs
  | otherwise = []

take' :: Int -> [a] -> [a] 
take' = flip (foldr (\ x f -> (\num -> if num>0 then x : f (num-1) else []) ) (\ _ -> []))

-- take' :: Int -> [a] -> [a] 
-- take' = flip takeAux
--     where
--         takeAux :: [a] -> (Int -> [a])
--         takeAux = foldr cons basecase
--             where 
--                 basecase :: Int -> [a]
--                 basecase = (\_ -> [])

--                 cons :: a -> (Int -> [a]) -> (Int -> [a]) 
--                 cons     = (\x f -> (\nums -> if nums>0 then x : f (nums-1) else [])) 