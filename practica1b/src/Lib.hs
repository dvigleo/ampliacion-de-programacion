module Lib
    ( gcd'
    ) where

gcd' a b
    | b /= 0 = gcd' b (a `mod` b)
    | otherwise = a
