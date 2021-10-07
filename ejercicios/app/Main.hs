{-# LANGUAGE RankNTypes #-}
module Main where

import Test.QuickCheck

{--
    h) Redefinición de las funciones originales para que sean polimórficas
--}

repetir :: Integer -> [Integer]
repetir x = x: repetir x

h1 = repetir 3

coger :: Integer -> [Integer] -> [Integer]
coger n _ | n <= 0 = []
coger _ [] = []
coger n (x:xs) = x : coger (n-1) xs

h2 = coger 1 [3, 4, 5]

{--
    i.a) Definción de las funciones coger y repetir a partir de 
    las nativas take y repeat
--}

repetiri :: Integer -> [Integer]
repetiri =  repeat

repetiri1 = repetiri 2

cogeri :: Int -> [Int] -> [Int]
cogeri n _ | n <= 0 = []
cogeri _ [] = []
cogeri n (x:xs) = x : take (n-1) xs

cogeri1 = cogeri 1 [9, 8, 7]

{--
    i.b) Ejemplos de fromInteger, toInteger, maxBound, Integral, 
    Bounded sobre las funciones de coger y repetir
--}

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

{--
    n) Utilizando la librería Test.QuickCheck,quickCheck para realizar 100 pruebas
--}
                                              
cogercomp :: Int -> [a] -> [a]
cogercomp n _ | n <= 0 = []
cogercomp _ [] = []
cogercomp n (x:xs) = x : cogercomp (n-1) xs

comprobarCoger:: Num a => (Int, [a]) -> Bool
comprobarCoger (x1, x2) = take x1 x2 == cogercomp x1 x2
                            && cogercomp x1 x2 == take x1 x2

comprobacion = quickCheck comprobarCoger
    where valores = [(x, y) | x<-[1..2], y<-['A'..'Z']]

main :: IO ()
main = 
    print comprobacion
--    print a
--    print b
--    print c
--    print d
--    print e
--    print f
--    print repetiri1
--    print cogeri1
--    print h2
--    print i2
--    print i4