module MainApartadoL (mainApartadoL) where

import           Func.Definiciones

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

mainApartadoL :: IO ()
mainApartadoL = do
    print l
    print l'
    print l''
