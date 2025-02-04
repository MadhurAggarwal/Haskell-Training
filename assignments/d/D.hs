---------------------------------------------------------------------
-- D.hs
--
-- (c) 2017-2021 Andres Loeh, Well-Typed LLP

{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFoldable #-}
module D where

import Data.List (sortBy)
import Prelude hiding (head, tail)
import Text.Read (readMaybe)

-- Task D-1:
--
-- Define a function 'classify' that decides whether
-- a given string does in fact parse as an integer.
--
-- Use the function 'readMaybe'.
--
-- Examples:
--
-- >>> classify " 34   "
-- AnInt 34
-- >>> classify " - 1"
-- AnInt (-1)
-- >>> classify "hello"
-- AString "hello"
-- >>> classify "1!!!1"
-- AString "1!!!1"
-- >>> classify "42"
-- AnInt 42
--

data IntOrString =
    AnInt Int
  | AString String
  deriving (Show, Read)

classify :: String -> IntOrString
classify str =
    case readMaybe str of
      Nothing -> AString str
      Just x  -> AnInt x

-- Task D-2.
--
-- Define an 'Eq' instance for IntOrString that
-- deviates from the standard equality in that it
-- considers any two strings equal, but distinguishes
-- between different integers.
--
-- Examples:
--
-- >>> AnInt 3 == AnInt 5
-- False
-- >>> AnInt 4 == AnInt 4
-- True
-- >>> AnInt 7 == AString "foo"
-- False
-- >>> AString "bar" == AnInt (-12)
-- False
-- >>> AString "foo" == AString "bar"
-- True
-- >>> AString "hello" == AString ""
-- True
--

-- TODO: Define Eq instance for IntOrString
instance Eq IntOrString where
  (==) :: IntOrString -> IntOrString -> Bool
  (AnInt x)   == (AnInt y)   = x == y
  (AString _) == (AString _) = True
  _           == _           = False

-- Task D-3.
--
-- We define non-empty lists to be plain lists that
-- have one guaranteed first element.
--
--   data NonEmpty a = a :| [a]
--
-- The constructor (:|) is an infix constructor, so
-- the type of the constructor is
--
--   (:|) :: a -> [a] -> NonEmpty a
--
-- Define selector functions head and tail on this
-- type. Give the type signatures yourself. Note that
-- unlike for plain lists, these selector functions
-- are total for non-empty lists.
--
-- (This type is available in the base package from
-- the module Data.List.NonEmpty, but we are defining
-- our own version here.)

data NonEmpty a = a :| [a]
  deriving (Show,Foldable)

head :: NonEmpty a -> a
head (x :| _)  = x

tail :: NonEmpty a -> [a]
tail (_ :| xs) = xs

-- Task D-4.
--
-- Define a function 'cons' that prepends a single
-- element to a non-empty list.
--

cons :: a -> NonEmpty a -> NonEmpty a
cons val (x :| xs) = val :| (x:xs)

-- Task D-5.
--
-- Define a function 'group' that turns a list of
-- elements into a list of non-empty list (groups)
-- where each list contains adjacent equal elements
-- of the original list.
--
-- Examples:
--
-- >>> group [1,1,0,2,2,2]
-- [1 :| [1],0 :| [],2 :| [2,2]]
-- >>> group "hello"
-- ['h' :| "",'e' :| "",'l' :| "l",'o' :| ""]
-- >>> group "aaaaxxaaaax"
-- ['a' :| "aaa",'x' :| "x",'a' :| "aaa",'x' :| ""]

group :: Eq a => [a] -> [NonEmpty a]
group [] = []
group (x : []) = [x :| []]
group (x : y : ys) = if x == y then cons x one : res else (x :| []) : one : res
  where
    one : res = group (y : ys)

{-
  I get non-exhaustive warning on where clause in GHC? The pattern is exhaustive.
  The GHC warning is that it cannot match to [], but it can: 
  if we have ys = [] 
    then group (y:[]) = [y :| []] = (y :| []) : []
    which is of the type one : res 
  So the pattern is exhaustive
-}

-- Task D-6.
--
-- Without deriving the instance, make NonEmpty an
-- instance of the Eq class, defining equality in the
-- same way as the derived instance would compute it.

-- TODO: Define Eq instance for NonEmpty
instance Eq a => Eq (NonEmpty a) where
  (==) :: Eq a => NonEmpty a -> NonEmpty a -> Bool
  (x :| xs) == (y :| ys) = (x == y) && (xs == ys)

-- Task D-7:
--
-- Without deriving the instance, make NonEmpty an
-- instance of the Functor class, which means you will
-- have to define an fmap function for the NonEmpty
-- datatype.

-- TODO: Define Functor instance for NonEmpty
instance Functor NonEmpty where
  fmap :: (a->b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| fmap f xs

-- Task D-8:
--
-- Derive a Foldable instance for the NonEmpty datatype.
-- For this, you can modify the data declaration above.
--
-- (Note that for this you will have to enable the
-- DeriveFoldable language extension for this module.)
--
-- Convince yourself that functions such as
-- 'toList', 'length', 'sum' and others work correctly.
--
-- What can you say about the 'null' function?
--
-- PLEASE ANSWER THE QUESTIONS HERE
{-
    Using {-# LANGUAGE DeriveFoldable #-} and deriving Foldable we have made NonEmpty an instance of Foldable type.
    we have:
    Data.Foldable.toList (7 :| [1,2,3]) = [7,1,2,3]
    sum    (7 :| [1,2,3])               = 13
    length (7 :| [1,2,3])               = 4
    null   (7 :| [1,2,3])               = False
    So, yes all the functions work.

    Here, null is ALWAYS False.
    NonEmpty type has length >= 1 as per definition. Hence, it cannot be empty or null. 
    we can write 
    null :: NonEmpty -> Bool
    null _ = False
-}


-- Task D-9:
--
-- Use the function 'sortBy' from the Data.List module
-- to define a function 'sortDescending' that sorts a
-- list in descending rather than ascending order, and
-- a function 'sortSnds' that sorts a lists of pairs
-- based on the second components only.
--
-- Example:
--
-- >>> sortDescending [1,7,3,4,6,4,5]
-- [7,6,5,4,4,3,1]
-- >>> sortDescending "hello world"
-- "wroolllhed "
-- >>> sortSnds [(1,7), (3,4), (7,6), (5,2), (2,6), (9,1)]
-- [(9,1),(5,2),(3,4),(7,6),(2,6),(1,7)]
-- >>> sortSnds [(True, 2.5), (False, 4.5), (True, 2.7), (False, 9.234)]
-- [(True,2.5),(True,2.7),(False,4.5),(False,9.234)]
--

sortDescending :: Ord a => [a] -> [a]
sortDescending = sortBy (flip compare)

sortSnds :: Ord b => [(a,b)] -> [(a,b)]
sortSnds = sortBy (\(_,x) (_,y) -> compare x y)
