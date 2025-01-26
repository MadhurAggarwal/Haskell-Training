module Main where

import System.Environment (getExecutablePath)
main :: IO ()
-- main = writeFile "mypractice/txtfiles/first.txt" "Hello!\n\t😁\n"

main = do
    path <- getExecutablePath
    putStrLn path