module Main where

import Lib

a::String
a = "Cadena"

b::Int
b = length a + 7

c::Integer
c = 3

d::Num a => a
d = 3

e::Num a => (Integer, a)
e = (c, d)

f::[Integer]
f = [c,d]

repetir :: Integer -> [Integer]
repetir x = x: repetir x

h1 = repetir 3

coger :: Integer -> [Integer] -> [Integer]
coger n _ | n <= 0 = []
coger _ [] = []
coger n (x:xs) = x : coger (n-1) xs

h2 = coger 1 [3, 4, 5]

i1::Integer
i1 = 4
i2::Int
i2 = fromInteger i1
i3::Integer
i3 = toInteger i2
i4 = maxBound::Int

repetir2 :: Integer -> [Integer]
repetir2 x = fromInteger x: repetir x

i5 = repetir2 3

coger2 :: Integer -> [Integer] -> [Integer]
coger2 n _ | fromInteger n <= 0 = []
coger2 _ [] = []
coger2 n (x:xs) = fromInteger x : coger (n-1) xs

i6 = coger2 1 [3, 4, 5]

coger3 :: Integer -> [Integer] -> [Integer]
coger3 n _ | fromInteger n <= 0 = []
coger3 _ [] = []
coger3 n (x:xs) = fromInteger x : coger (n-1) xs

l = coger3 1 [3, 4, 5]

main :: IO ()
main = do
    print a
    print b
    print c
    print d
    print e
    print f
    print h2
    print i2
    print i4
