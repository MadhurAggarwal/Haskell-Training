module Parametricity where 

import Prelude hiding (fst, map)

fst :: (a,b) -> a
fst (x,_) = x

loop :: a 
loop = loop 

map :: (a->b) -> [a] -> [b]
map _ []     = [] 
map f (x:xs) = f x : (map f xs)

-- parse :: String -> a 
-- parse "False" = False 
-- parse "0" = 0

