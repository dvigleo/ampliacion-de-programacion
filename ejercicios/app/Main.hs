{-# LANGUAGE RankNTypes #-}
module Main where

import           Debug.Trace
import           Test.QuickCheck

{-
    h) Redefinición de las funciones originales para que sean polimórficas
-}

repetir :: Integer -> [Integer]
repetir x = x: repetir x

h1 = repetir 3

coger :: Integer -> [Integer] -> [Integer]
coger n _      | n <= 0 = []
coger _ []     = []
coger n (x:xs) = x : coger (n-1) xs

h2 = coger 1 [3, 4, 5]

{-
    i.a) Definción de las funciones coger y repetir a partir de
    las nativas take y repeat
-}

repetiri :: Integer -> [Integer]
repetiri =  repeat

repetiri1 = repetiri 2

cogeri :: Int -> [Int] -> [Int]
cogeri n _      | n <= 0 = []
cogeri _ []     = []
cogeri n (x:xs) = x : take (n-1) xs

cogeri1 = cogeri 1 [9, 8, 7]

{-
    i.b) Ejemplos de fromInteger, toInteger, maxBound, Integral,
    Bounded sobre las funciones de coger y repetir
-}

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
coger2 n _      | fromInteger n <= 0 = []
coger2 _ []     = []
coger2 n (x:xs) = fromInteger x : coger (n-1) xs

i6 = coger2 1 [3, 4, 5]

{-
    l) Redefinir la función coger para que obtenga expresiones de tipo Maybe
-}
cogerMaybe :: Integer -> [Integer] -> Maybe [Integer]
cogerMaybe n _      | n <= 0 = Nothing
cogerMaybe _ []     = Nothing
cogerMaybe n (x:xs) = Just (x : coger (n-1) xs)

l = cogerMaybe 2 [0, 1, 2, 3, 4]
l' = cogerMaybe (-2) [0, 1, 2, 3, 4]
l'' = cogerMaybe 1 []

{-
    n) Utilizando la librería Test.QuickCheck,quickCheck para realizar 100 pruebas
-}

cogerABC :: Int -> [Char] -> [Char]
cogerABC n _      | n <= 0 = []
cogerABC _ []     = []
cogerABC n (x:xs) = x : cogerABC (n-1) xs

comprobarCoger :: Int -> [Char] -> Bool
comprobarCoger x xs = take x xs == cogerABC x xs
    && cogerABC x xs == take x xs

comprobacionN :: IO ()
comprobacionN = quickCheck comprobarCoger

{-
    o) Usar la función trace para la ejecución recursiva de la función coger
    sobre un ejemplo concreto, mostrando en pantalla los diferentes resultados
    de ejecución de la función coger. Además, explicar al estilo de la
    diapositiva 16 del fichero 03-introduccion-hs.ppt, cuál sería la cadena
    de evaluaciones a partir de la expresión de partida
-}

traceCoger :: Integer -> [Integer] -> [Integer]
traceCoger x xs = trace ("DEBUG coger " ++ show x ++" | " ++ show xs) coger x xs

main :: IO ()
main = do
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
    -- print l
    -- print l'
    -- print l''
    comprobacionN
    print $ traceCoger 2 [0, 1, 2, 3, 4]
