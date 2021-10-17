module MainApartadoN (mainApartadoN) where

import           Func.Definiciones
import           Test.QuickCheck

{-
    n) RUsando stack y VSC, utilizar la función Test.QuickCheck.quickCheck
    para la comprobación mediante 100 pruebas que la función coger obtiene
    el mismo resultado que la función take para diferentes cantidades de
    elementos del alfabeto en mayúsculas
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

mainApartadoN :: IO ()
mainApartadoN = comprobacionN
