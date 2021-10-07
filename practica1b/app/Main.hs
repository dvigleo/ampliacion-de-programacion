module Main where

import Lib
{-
    a) Implementar el problema de referencia considerando que todos
    los números usados son del tipo Integer y son representados como
    una tupla de 2 números
-}

type Fraccion = (Integer, Integer)
f1 :: Fraccion
f1 = (2, 3)

{-
    b) Implementar el problema de referencia usando data para definir
    el tipo Fraccion', permitiendo definir una fracción con 3
    constructores, de los cuales uno de ellos sea un operador,
    y los otros dos contengan 1 y 2 parámetros
-}

data Fraccion' =
    F1 Integer Integer  |
    F2 Integer          |
    Integer :/ Integer
f1', f2', f3'  :: Fraccion'
f1' = F1 2 3
f2' = F2 4
f3' = 3 :/ 4

-- 1) simplificar al máximo una fracción
simp1' num den = ((signum den * num) `div` m, abs den `div` m)
    where m = gcd' num den

-- 2) comparar 2 fracciones
comp2' :: Ord a => a -> a -> String
comp2' frac1 frac2 
    | frac1 == frac2 = "Son iguales"
    | frac1 > frac2 = "La primera fraccion es mayor"
    | otherwise = "La segunda fraccion es mayor"

-- 3) sumar varias fracciones
-- 4) multiplicar 2 fracciones
mult4' frac1 frac2 = frac1 * frac2

-- 5) restar 2 fracciones
resta5' frac1 frac2 = (-) frac1 frac2

-- 6) restar y multiplicar varias fracciones (poniendo paréntesis 
    -- desde izquierda y desde derecha)
-- 7) convertir a cadena una fracción
cadena7' frac = show frac
-- 8) convertir a cadena una lista de fracciones
cadena80 listafracs = show listafracs
-- 9) buscar entre varias fracciones las que son positivas, negativas y nulas
-- 10) quitar de entre varias fracciones las equivalentes a una inicial
-- 11) ordenar en modo creciente una lista de fracciones
-- 12) obtener todas las sumas posibles a partir de 2 listas de fracciones 
    -- considerando una fracción de la primera lista y otra fracción de la 
    -- segunda, obtener todas las fracciones equivalentes a partir de una inicial
-- 13) simplificar al máximo los elementos de una lista de fracciones y eliminar las equivalentes.

main :: IO ()
main = do 
    putStrLn "\n\n ------------- Apartado b) ------------- \n"
    putStrLn "Simplificador de fracciones:"
    print $ simp1' 2 4
    putStrLn "\nComparación de fracciones:"
    print $ comp2' 1 1
    print $ comp2' 2 1
    print $ comp2' 1 2
    putStrLn "\nMult. de 2 fracciones:"
    print $ mult4' 1 1
    putStrLn "\nResta de 2 fracciones:"
    print $ resta5' 1 1
    putStrLn "\nConvertir a cadena una fracción:"
    print $ cadena7' (1, 2)
    putStrLn "\nConvertir a cadena una lista de fracciones:"
    print $ cadena7' [(1, 2), (2, 3), (3, 4)]
