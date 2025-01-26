module InteractionTree where

import Data.Char
import System.IO

-- A Question String, and an Interaction Tree for Yes and one for No
-- we can use List of Interactions instead for MCQ questions
data Interaction = Result String | Question String Interaction Interaction
    deriving Show 

example :: Interaction 
example = 
    Question "Do You Like Static Types?"
        (Question "Do You Like Linear or Affine Type?"
            (Result "Try Rust!")
            (Question "Do You Like Dependant Types?"
                (Result "Try Idris!")
                (Result "Try Ocamel!")
            )
        )
        (Question "Do You Like Parentheses?"
            (Result "Try Clojure!")
            (Result "Try Erlang!")
        )

runInteraction :: Interaction -> IO ()
runInteraction (Result str)       = putStrLn str 
runInteraction (Question str a b) = do
    res <- askYesNo str
    if res 
        then runInteraction a 
        else runInteraction b

askYesNo :: String -> IO Bool 
askYesNo str = do
    putStr (str ++ " [y|n] ")
    res <- getChar
    hFlush stdout
    putStrLn ""
    if res == 'y'
        then return True
        else if res == 'n' 
            then return False
            else askYesNo str

    -- res <- getLine
    -- if (map toUpper res) `elem` ["YES","Y","T","True","Correct"]
    --     then return True 
    --     else return False

simulate :: Interation -> [Bool] -> Maybe String
simulate (Result str)     []       = Just str 
simulate (Question _ y n) (x : xs) = if x then simulate y xs else similate n xs
simulate _                _        = Nothing



