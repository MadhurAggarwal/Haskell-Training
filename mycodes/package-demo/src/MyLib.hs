{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
module MyLib where

import System.Random
import System.Directory
import Data.Time
import Control.Monad
import System.IO.Error
import Control.Exception

filesInCurrentDirectory :: IO [FilePath]
filesInCurrentDirectory = do 
    content <- getDirectoryContents "."
    filterM doesFileExist content

filesInDirectory :: FilePath -> IO [FilePath]
filesInDirectory path = do
    content <- getDirectoryContents path
    filterM (\ name -> doesFileExist (path ++ "/" ++ name)) content 

fileSizesInDirectory :: FilePath -> IO [(FilePath, Integer)]
fileSizesInDirectory path = do
    content <- filesInDirectory path
    mapM (\name -> fmap ((,) name) (getFileSize (path ++ "/" ++ name))) content

newFileInNewDirectory :: FilePath -> String -> String -> IO ()
newFileInNewDirectory dirName fileName fileContent = do
    createDirectory dirName 
    writeFile (dirName ++ "/" ++ fileName) fileContent 


main :: IO ()
main = createDirectory "abc" >> writeFile "abc/test.txt" "Helsdjnv"