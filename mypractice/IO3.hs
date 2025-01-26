module IO3 where

-- import qualified Prelude
-- import Prelude hiding ((>>))
-- this means we have Prelude.(>>) available and rest are avaible directly without writing Prelude. before the name

import qualified Control.Applicative
import Data.Char

liftA2 :: (a -> b -> c) -> IO a -> IO b -> IO c 
liftA2 = Control.Applicative.liftA2

-- (>>) :: IO a -> IO b -> IO b
-- (>>) = liftA2 (\ _ b -> b)

-- Program 1 (IO.hs)
program1 :: IO String 
program1 = liftA2 (++) getLine getLine 

program2 :: IO String 
program2 = fmap (\x -> x ++ x) getLine 

program3 :: IO String 
program3 = liftA2 (\ x y -> y ++ x) getLine getLine

countChars :: IO Int 
countChars = fmap length getLine

secondLineOnly :: IO String 
secondLineOnly = Control.Applicative.liftA3 (\_ a _ -> a) getLine getLine getLine

echo :: IO ()
echo = getLine >>= putStrLn

-- (>>=) :: IO a -> (a -> IO b) -> IO b 
-- getLine :: IO String
-- putStrLn :: String -> IO ()
-- getLine >>= :: (a -> IO b) -> IO b

echoReverse :: IO ()
echoReverse = getLine >>= \x -> putStrLn (reverse x)

echoTwice :: IO ()
echoTwice = getLine >>= \x -> putStrLn x >> putStrLn x

shout :: IO ()
shout = getLine >>= \x -> let y = map toUpper x in putStrLn y >> putStrLn y

shout' :: IO ()
shout' = do 
    x <- getLine
    y <- return (map toUpper x)
    putStrLn y 
    putStrLn y

shout'' :: IO ()
shout'' = do 
    x <- getLine
    let y = map toUpper x 
    putStrLn y 
    putStrLn y

combineLines :: IO ()
combineLines = getLine >>= \x -> getLine >>= \y -> putStrLn (x ++ ", " ++ y)

combineLine' :: IO ()
combineLine' = helper >>= \x -> putStrLn x 
    where 
        helper :: IO String 
        helper = liftA2 (\x y -> x ++ ", " ++ y) getLine getLine

-- (>>) :: IO a -> IO b -> IO b 
-- ioa >> iob = ioa >>= (\_ -> iob)

-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- return :: a -> IO a

fmap' :: (a -> b) -> IO a -> IO b 
fmap' f ioa = ioa >>= \a -> return (f a)

liftA2' :: (a -> b -> c) -> IO a -> IO b -> IO c 
liftA2' f ioa iob = 
    ioa >>= \a -> 
    iob >>= \b -> 
    return (f a b)

sizeOfFile :: FilePath -> IO ()
sizeOfFile path = readFile path >>= \str -> putStrLn ("Length is: " ++ show (length str))

forever :: IO a -> IO b
forever ioa = ioa >> forever ioa

liftA3' :: (a -> b -> c -> d) -> IO a -> IO b -> IO c -> IO d
liftA3' f ioa iob ioc = 
    ioa >>= \a -> 
    iob >>= \b ->
    ioc >>= \c ->
    return (f a b c)

conversation :: IO ()
conversation = 
    putStrLn "Please Provide Input: " >>
    getLine >>= \x ->
    if null x 
        then putStrLn "Bye Bye!"
        else putStrLn ("Thanks For the Input: (" ++ x ++ "). ") >> conversation

conversation1 :: IO ()
conversation1 = 
    ask "Please Provide Some Input: " >>= \x ->
    if null x 
        then putStrLn "Bye Bye!"
        else putStrLn ("Thanks For the Input: (" ++ x ++ "). ") >> conversation1

ask :: String -> IO String
ask prompt = 
    putStrLn prompt >>
    getLine

conversation' :: IO ()
conversation' = do 
    x <- ask "Please Provide Some Input: "
    if null x 
        then putStrLn "Bye Bye!"
        else do 
            putStrLn ("Thanks For the Input: (" ++ x ++ "). ")
            conversation'

liftA2'' :: (a -> b -> c) -> IO a -> IO b -> IO c
liftA2'' f ioa iob = do
    a <- ioa
    b <- iob 
    return (f a b)

question :: String -> IO ()
question q = putStrLn (q ++ "?")