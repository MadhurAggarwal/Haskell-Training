module IO where

import Prelude hiding (getLine)

getLine :: String
getLine = error "wtf"

-- Program 1
-- Read a Line, Read Another Line, Return Appended Strings

program1 :: String
program1 = 
    let 
        x = getLine 
        y = getLine 
    in 
        x ++ y

program1' :: String
program1' = getLine ++ getLine

-- Program 2
-- Read a Line, Return the string appended to itself

program2 :: String 
program2 = 
    let 
        x = getLine 
    in 
        x ++ x

program2' :: String
program2' = getLine ++ getLine

-- Program 3
-- Read a Line, Read another line, Return Appended Strings in flipped order

program3 :: String 
program3 = 
    let
        x = getLine 
        y = getLine 
    in 
        y ++ x

program3' :: String
program3' = getLine ++ getLine

-- As per Lazy Evaluation, the three programs are all same