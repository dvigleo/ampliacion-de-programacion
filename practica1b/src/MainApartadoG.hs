{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}

module MainApartadoG
    ( mainApartadoG ) where

{---------------------------------------------------------------------
    e) A partir del apartado c), crear una clase conteniendo un método
    que genere todas las posibles sumas a partir de 2 listas.
    Los elementos de cada lista deben ser de un tipo perteneciente a
    la clase Num. Hacer que el tipo de las fracciones pertenezca
    a esa clase, y también los tipos Integer y Double.
----------------------------------------------------------------------}

data (Integral a) => Fraccion a =
    F1 a a | F2 a | a :/ a
    deriving (Show)
class SumarListas a where
    (+%+) :: [a] -> [a] -> [a]
instance (Integral a) => SumarListas (Fraccion a) where
    xs +%+ ys = [sumarFracciones x y | x <- xs, y <- ys]

-- data Fraccion =
--     F1 Integer Integer |
--     F2 Integer         |
--     Integer :/ Integer
--     deriving Show
-- instance Num [Fraccion] where
--   (+) xs ys = [sumarFracciones x y | x <- xs, y <- ys]

sumarFracciones (F1 a b) (F1 c d) = F1 (a * (y `div` b) + c * (y `div` d)) y
    where y = (b * d) `div` gcd b d
sumarFracciones (F1 a b) (F2 c) =  F1 (a * (y `div` b) + c * y) y
    where y = b `div` gcd b 1
sumarFracciones (F1 a b) (c :/ d) = (a * (y `div` b) + c * (y `div` d)) :/ y
    where y = (b * d) `div` gcd b d

sumarFracciones (a :/ b) (c :/ d) =  (a * (y `div` b) + c * (y `div` d)) :/ y
    where y = (b * d) `div` gcd b d
sumarFracciones (a :/ b) (F1 c d) = (a * (y `div` b) + c * (y `div` d)) :/ y
    where y = (b * d) `div` gcd b d
sumarFracciones (a :/ b) (F2 c) = (a * (y `div` b) + c * y) :/ y
    where y = b `div` gcd b 1

sumarFracciones (F2 a) (F2 b) = F2 (a + b)
sumarFracciones (F2 a) (F1 c d) = F1 (a * y + c * (y `div` d)) y
    where y = d `div` gcd 1 d
sumarFracciones (F2 a) (c :/ d) = (a * y + c * (y `div` d)) :/ y
    where y = d `div` gcd 1 d

lista3, lista4 :: [Fraccion Integer]
-- lista3, lista4 :: [Fraccion]
lista3 = [F1 4 1, F2 5]
lista4 = [1 :/ 2, F2 8]


mainApartadoG :: IO ()
mainApartadoG = print $ lista3 +%+ lista4
    -- print $ lista3 + lista4

{---------------------------------------------------------------------
    RESULTADO DEL PROGRAMA

[9 :/ 2,F1 12 1,11 :/ 2,F2 13]
---------------------------------------------------------------------}
