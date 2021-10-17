module MainApartadoH (mainApartadoH) where

{-
    h) Redefinir las funciones repetir y coger del apartado anterior para que
    sean monomórficas sobre 2 tipos particulares, y no polimórficas, incluyendo
    también ejemplos de uso.
-}

coger :: Integer -> [Integer] -> [Integer]
coger n _      | n <= 0 = []
coger _ []     = []
coger n (x:xs) = x : coger (n-1) xs

repetir :: Integer -> [Integer]
repetir x = x: repetir x

mainApartadoH :: IO ()
mainApartadoH = do
    print $ coger 2 [0, 1, 2, 3, 4, 5]
    print $ coger 10 (repetir 6)
    -- print $ coger 2 ['a', 'b', 'c'] -- ERROR, la función coger espera recibir un tipo de dato Integer
    -- print $ coger 2 (repetir 'c') -- ERROR la función repetir espera recibir un Integer
