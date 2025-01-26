module MapFilterIO where 

import Control.Applicative

import Prelude hiding (sequence)

ask :: String -> IO String 
ask prompt = do 
    putStrLn prompt
    getLine

askMany :: [String] -> IO [String]
askMany []       = return []
askMany (q : qs) = do 
    ans <- ask q 
    res <- askMany qs
    return (ans : res)

askMany' :: [String] -> IO [String]
askMany' []       = return []
askMany' (q : qs) = liftA2 (:) (ask q) (askMany' qs)

askMany'' :: [String] -> IO [String]
askMany'' = sequence . map ask 

sequence :: [IO a] -> IO [a]
sequence []       = return []
sequence (x : xs) = do 
    a   <- x 
    res <- sequence xs 
    return (a : res)

sequence' :: [IO a] -> IO [a]
sequence' []       = return []
sequence' (x : xs) = liftA2 (:) x (sequence' xs)


filterM' :: (a -> IO Bool) -> [a] -> IO [a]
filterM' f []       = return []
filterM' f (x : xs) = do
    a   <- f x 
    res <- filterM' f xs
    if a 
        then return (x : res)
        else return res

filterM'' :: (a -> IO Bool) -> [a] -> IO [a]
filterM'' f []       = return []
filterM'' f (x : xs) = do
    a   <- f x 
    if a 
        then fmap (x :) (filterM'' f xs)
        else filterM'' f xs

askNull :: String -> IO Bool
askNull q = do 
    a <- ask q 
    return (null a)

print' :: Show a => a -> IO ()
print' = putStrLn . show 

    
