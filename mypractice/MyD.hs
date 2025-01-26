{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
module MyD where

import Data.List (sortBy)
import Prelude hiding (head, tail)
import Text.Read (readMaybe)

data IntOrString =
    AnInt Int
  | AString String
  deriving (Show, Read)

classify :: String -> IntOrString
classify str = 
    case readMaybe str :: Maybe Int of 
        Nothing -> AString str
        (Just x)  -> AnInt x


head :: NonEmpty a -> a 
head (x :| _) = x

tail :: NonEmpty a -> a
tail (x :| []) = x
tail (x :| xs) = tail 

-- var = [] == []