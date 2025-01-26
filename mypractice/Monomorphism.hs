{-# OPTIONS_GHC -Wall #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Monomorphism where

minimal :: Bounded a => a
minimal = minBound

equalToItself :: Eq a => a -> Bool
equalToItself = \x -> x == x

equalToItself' x = x==x 

default (Rational)
x = 2
y = 2.5

strange x = show (read x)