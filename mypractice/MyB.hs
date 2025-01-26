module MyB where

import Prelude hiding (or, reverse, filter, replicate, repeat)
import Control.Applicative ((<|>), liftA2)

-- 1
collapse :: Either [Int] Int -> Int
collapse (Left a) = sum a
collapse (Right b) = b

-- 2
newtype Month = MkMonth Int
  deriving Show

mkMonth :: Int -> Maybe Month
mkMonth a = if elem a [1..12] then Just (MkMonth a) else Nothing

-- 3
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

-- 4
pairMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe (Just a) (Just b) = Just (a,b)
pairMaybe _ _ = Nothing

-- 5
liftMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe f (Just a) (Just b) = Just (f a b)
liftMaybe _ _ _ = Nothing

-- 6
pairMaybe' :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe' a b = liftMaybe (,) a b 

-- 7
lookups :: Eq a => [a] -> [(a, b)] -> Maybe [b]
lookups [] table = Just []
lookups (x:xs) table = 
    case lookup x table of 
        (Just a)  -> liftMaybe (:) (Just a) (lookups xs table)
        otherwise -> Nothing

table = [(1, "x"), (2, "y"), (3, "z")]

-- 8
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- 9
reverseAcc :: [a] -> [a] -> [a]
reverseAcc x [] = x
reverseAcc x (y:ys) = reverseAcc (y:x) ys

-- 10
{-
    reverse' is much faster than reverse.
    reverse first traverses each list element by element, then appends a new element to a list.
    Both of these are O(n) operations, so reverse becomes Quadratic O(n2) operation
    reverseAcc (and hence reverse') traverses list only once, it is O(n) operation. 
    Hence the time difference due to different time complexities.
 -}

reverse' :: [a] -> [a]
reverse' = reverseAcc [] 

-- 11 
replicate :: Int -> a -> [a]
replicate a b = if a>0 then b : replicate (a-1) b else []

-- 12
repeat :: a -> [a]
repeat a = a : repeat a

-- 13
replicate' :: Int -> a -> [a]
replicate' a b = take a (repeat b)

-- 14
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node x y) = Node (mapTree f x) (mapTree f y)

-- 15
sameShape :: Tree a -> Tree b -> Bool
sameShape (Leaf a) (Leaf b) = True
sameShape (Node a1 a2) (Node b1 b2) = (sameShape a1 b1) && (sameShape a2 b2)
sameShape _ _ = False

tree1 :: Tree Int
tree1 = Leaf 1

tree2 :: Tree Int
tree2 = Node (Leaf 2) (Leaf 4)

tree3 :: Tree Int
tree3 = Node tree2 tree1

tree4 :: Tree Int
tree4 = Node tree2 tree3

tree5 :: Tree Int
tree5 = Node (Leaf 3) (Leaf 3)

-- 16
sameShape' :: Tree a -> Tree b -> Bool
f = (\x -> ())
sameShape' x y = mapTree f x == mapTree f y

-- 17
buildTree :: Int -> Tree ()
buildTree a = if a>0 then Node (buildTree (a-1)) (buildTree (a-1)) else Leaf ()

-- 18
data Expr =
    Lit Int
  | Add Expr Expr
  | Neg Expr
  | IfZero Expr Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show)

expr1 :: Expr
expr1 = Neg (Add (Lit 3) (Lit 5))

expr2 :: Expr
expr2 = IfZero expr1 (Lit 1) (Lit 0)

prop_eval1 :: Bool
prop_eval1 = eval expr1 == -8

prop_eval2 :: Bool
prop_eval2 = eval expr2 == 0

eval :: Expr -> Int
eval (Lit x)        = x
eval (Add x y)      = eval x + eval y
eval (Neg x)        = (-1)*(eval x)
eval (IfZero x y z) = if eval x==0 then eval y else eval z
eval (Mul x y)      = eval x * eval y

-- 19
countOps :: Expr -> Int
countOps (Lit _)        = 0
countOps (Add x y)      = 1 + countOps x + countOps y
countOps (Neg x)        = 1 + countOps x
countOps (IfZero x y z) = 1 + countOps x + countOps y + countOps z
countOps (Mul x y)      = 1 + countOps x + countOps y

-- 20
-- Added changes to above implementations

-- 21
data Path = EndOfPath | LeftPath Path | RightPath Path
  deriving (Eq, Show)

-- 22
follow :: Path -> Tree a -> Maybe a
follow EndOfPath (Leaf a) = Just a
follow (LeftPath a)  (Node l _) = follow a l 
follow (RightPath a) (Node _ r) = follow a r
follow _ _ = Nothing

-- 23
search :: Eq a => a -> Tree a -> Maybe Path
search a (Leaf b)   = if a==b then Just EndOfPath else Nothing 
search a (Node l r) = fmap LeftPath (search a l) <|> fmap RightPath (search a r)