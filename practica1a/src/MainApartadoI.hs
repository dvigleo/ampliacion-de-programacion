module MainApartadoI (mainApartadoI) where

import           Func.Definiciones

{-
    i) ¿Cómo se podrían definir las funciones coger y repetir a partir de las
    nativas take y de repeat?. Explicar con ejemplos qué son fromInteger,
    toInteger, maxBound, Integral, Bounded en el contexto de los tipos
    Int e Integer y usando ejemplos sobre las funciones take y repeat.

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

mainApartadoI :: IO ()
mainApartadoI = do
    putStrLn " Apartado i.a) "
    -- print repetiri1
    print cogeri1
    putStrLn " Apartado i.b) "
    print i1
    print i2
    print i3
    print i4
    -- print i5
    print i6
