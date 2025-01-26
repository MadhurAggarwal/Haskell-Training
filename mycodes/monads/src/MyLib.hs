{-# LANGUAGE InstanceSigs #-}
module MyLib where

import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Monad

import Control.Monad.Trans.State

example :: M.Map Int Int
example =
    M.fromList
    [(1,2),
     (2,4),
     (3,6),
     (4,8),
     (5,10),
     (6,12),
     (8,16),
     (10,20),
     (15,30),
     (16,32),
     (20,40),
     (32,64)
    ]

ex2 :: M.Map Int Int
ex2 = M.singleton 5 10

-- Task 1
-- Starting from a given Integer
-- Perform 3 successive Lookups in table
-- Result of one lookup is input for next
-- Return final result + 1

-- if any one lookup fails, whole computation fails

threeLookups''' :: Int -> M.Map Int Int -> Maybe Int
threeLookups''' key table =
    case M.lookup key table of
        Nothing   -> Nothing
        Just key2 ->
            case M.lookup key2 table of
                Nothing   -> Nothing
                Just key3 ->
                    case M.lookup key3 table of
                        Nothing  -> Nothing
                        Just val -> Just (val + 1)

-- (>>=) :: IO a -> (a -> IO b) -> IO b

func :: Maybe a -> (a -> Maybe b) -> Maybe b
func computation rest =
    case computation of
        Nothing -> Nothing
        Just x  -> rest x

threeLookups' :: Int -> M.Map Int Int -> Maybe Int
threeLookups' key table =
    func
        (M.lookup key table)
        (\key2 ->
            func
                (M.lookup key2 table)
                (\key3 ->
                    func
                        (M.lookup key3 table)
                        (\val -> Just (val + 1))
                )
        )

(>>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>>=) computation rest =
    case computation of
        Nothing -> Nothing
        Just x  -> rest x

threeLookups'' :: Int -> M.Map Int Int -> Maybe Int
threeLookups'' key1 table =
    M.lookup key1 table  >>>= \key2 ->
    M.lookup key2 table >>>= \key3 ->
    M.lookup key3 table >>>= \val ->
        Just (val + 1)

threeLookups :: Int -> M.Map Int Int -> Maybe Int
threeLookups key table = do
    key2 <- M.lookup key  table
    key3 <- M.lookup key2 table
    val  <- M.lookup key3 table
    return (val + 1)

returnMaybe :: a -> Maybe a
returnMaybe = Just

-- sequence :: [IO a] -> IO [a]
-- sequence :: [Maybe a] -> Maybe [a]

lookups :: Ord a => [a] -> M.Map a b -> Maybe [b]
lookups keys table = mapM (\key -> M.lookup key table) keys

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f = sequence' . map f

sequence' :: Monad m => [m a] -> m [a]
sequence' []       = return []
sequence' (x : xs) = do
    a   <- x
    res <- sequence' xs
    return (a : res)

sequence'' :: Monad m => [m a] -> m [a]
sequence'' []       = return []
sequence'' (x : xs) = liftA2 (:) x (sequence'' xs)

data BinTree a = Empty | Bin (BinTree a) a (BinTree a)
    deriving (Show, Eq)

exampleTree :: BinTree Char
exampleTree =
    Bin
        (Bin
            (Bin Empty 'q' Empty)
            'a'
            Empty
        )
        'x'
        (Bin
            (Bin Empty 'h' Empty)
            'b'
            (Bin Empty 'r' Empty)
        )

labelTree' :: BinTree a -> BinTree (a, Int)
labelTree' tree = fst (labelTreeAux' tree 1)

labelTreeAux' :: BinTree a -> Int -> (BinTree (a, Int), Int)
labelTreeAux' Empty       num = (Empty, num)
labelTreeAux' (Bin l a r) num =
    let
        (lTree, lval) = labelTreeAux' l num
        (rTree, rval) = labelTreeAux' r (lval + 1)
    in
        (Bin lTree (a, lval) rTree, rval)

-- newtype Counter a = MkCounter (Int -> (a, Int))

-- runCounter :: Counter a -> (Int -> (a, Int))
-- runCounter (MkCounter f) = f




labelTree :: BinTree a -> BinTree (a, Int)
labelTree tree = fst (runCounter (labelTreeAux tree) 1 )

labelTreeAux'' :: BinTree a -> Counter (BinTree (a, Int))
labelTreeAux'' Empty       = MkCounter (\num -> (Empty, num))
labelTreeAux'' (Bin l a r) = MkCounter (\num -> 
        let 
            (ltree, lval)     = runCounter (labelTreeAux'' l) num
            (currVal, newVal) = runCounter  stepCounter       lval
            (rtree, rval)     = runCounter (labelTreeAux'' r) newVal
        in 
            (Bin ltree (a, currVal) rtree, rval)
    )

-- labelTreeAux :: BinTree a -> Counter (BinTree (a, Int))
-- labelTreeAux Empty       = MkCounter (\num -> (Empty, num))
-- labelTreeAux (Bin l a r) = 
--     labelTreeAux l >>>>= \ltree   -> 
--     stepCounter    >>>>= \currVal -> 
--     labelTreeAux r >>>>= \rTree   ->
--     returnCounter (Bin ltree (a, currVal) rTree)

newtype Counter a = MkCounter { runCounter :: Int -> (a, Int) }

stepCounter :: Counter Int 
stepCounter = MkCounter (\num -> (num, num + 1))

returnCounter :: a -> Counter a 
returnCounter x = MkCounter (\c -> (x,c))

(>>>>=) :: Counter a -> (a -> Counter b) -> Counter b
(>>>>=) baseCounter function =
    MkCounter (\ num ->
        let 
            (resA, val) = runCounter baseCounter num
        in 
            runCounter (function resA) val
    )

instance Functor Counter where
    fmap :: (a -> b) -> Counter a -> Counter b
    fmap = liftM 

instance Applicative Counter where 
    pure :: a -> Counter a
    pure = returnCounter
    (<*>) :: Counter (a -> b) -> Counter a -> Counter b
    (<*>) = ap

instance Monad Counter where
    (>>=) :: Counter a -> (a -> Counter b) -> Counter b
    (>>=) = (>>>>=)

-- liftM :: Monad m => (a -> b) -> m a -> m b 
-- liftM f computation = 
--     computation  >>= \a -> 
--     return (f a)

-- ap :: Monad m => m (a -> b) -> m a -> m b 
-- ap fun m_a = 
--     fun >>= \ aTob ->
--     m_a >>= \ a    ->
--     return (aTob a)

labelTreeAux :: BinTree a -> Counter (BinTree (a, Int))
labelTreeAux Empty       = return Empty
labelTreeAux (Bin l a r) = do
    ltree   <- labelTreeAux l
    currVal <- stepCounter
    rtree   <- labelTreeAux r
    return (Bin ltree (a, currVal) rtree)

-- newtype State s a = MkState {runState :: s -> (a,s)}
type Counter' = State Int 

-- (>>>>>=) :: State s a -> (a -> State s b) -> State s b 
-- (>>>>>=) initalState function = 
--     MkState (\ s0 -> 
--         let (a, s1) = runState initalState s0
--         in  runState (function a) s1
--     )

-- returnState :: a -> State s a  
-- returnState a = MkState (\s -> (a,s))

-- instance Functor (State s) where 
--     fmap = liftM 

-- instance Applicative (State s) where 
--     pure  = returnState 
--     (<*>) = ap 

-- instance Monad (State s) where 
--     (>>=) = (>>>>>=)

-- get :: State s s
-- get = MkState (\s -> (s, s))

-- put :: s -> State s () 
-- put s = MkState (\_ -> ((), s))

stepCounter' :: Counter' Int 
stepCounter' = do 
    s <- get 
    put (s + 1)
    return s

labelTreeFinal :: BinTree a -> BinTree (a, Int)
labelTreeFinal tree = evalState (labelTreeAuxFinal tree) 1

labelTreeAuxFinal :: BinTree a -> Counter' (BinTree (a, Int)) 
labelTreeAuxFinal Empty       = return Empty
labelTreeAuxFinal (Bin l a r) = do
    ltree   <- labelTreeAuxFinal l
    currVal <- stepCounter'
    rtree   <- labelTreeAuxFinal r
    return (Bin ltree (a, currVal) rtree)

