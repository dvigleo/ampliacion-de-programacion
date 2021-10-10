module Main where

-- import Lib
{-
    b) Implementar el problema de referencia usando data para definir
    el tipo Fraccion', permitiendo definir una fracción con 3
    constructores, de los cuales uno de ellos sea un operador,
    y los otros dos contengan 1 y 2 parámetros
-}

data Fraccion' =
    F1 Integer Integer |
    F2 Integer         |
    Integer :/ Integer
    deriving Show

-- 1) simplificar al máximo una fracción
simp1' :: Fraccion' -> Fraccion'
simp1' (F1 a b) = F1 ((signum b * a) `div` m) (abs b `div` m)
    where m = gcd a b

-- 2) comparar 2 fracciones
comp2' :: Fraccion' -> Fraccion' -> String
comp2' (F1 a b) (F1 c d)
    | y > 0 = "GT"
    | y < 0 = "LT"
    | otherwise = "EQ"
    where y = (a * d) - (b * c)
comp2' (F1 a b) (F2 c)
    | y > 0 = "GT"
    | y < 0 = "LT"
    | otherwise = "EQ"
    where y = a - (b * c)
comp2' (F2 a) (F1 c d)
    | y > 0 = "GT"
    | y < 0 = "LT"
    | otherwise = "EQ"
    where y = (a * d) - c
comp2' (F2 a) (F2 b)
    | y > 0 = "GT"
    | y < 0 = "LT"
    | otherwise = "EQ"
    where y = a - b

-- 3) sumar varias fracciones
sumaAux' :: Fraccion' -> Fraccion' -> Fraccion'
sumaAux' (F1 a b) (F1 c d) = simp1' (F1 (a * (y `div` b) + c * (y `div` d)) y)
    where y = (b * d) `div` gcd b d
sumaAux' (F1 a b) (F2 c) = simp1' (F1 (a * (y `div` b) + c * y) y)
    where y = b `div` gcd b 1
sumaAux' (F2 a) (F1 c d) = simp1' (F1 (a * y + c * (y `div` d)) y)
    where y = d `div` gcd 1 d
sumaAux' (F2 a) (F2 b) = F2 (a + b)

suma3' :: [Fraccion'] -> [Fraccion']
suma3' (x:y:xs) = if null xs
                    then [sumaAux' x y]
                    else suma3' (sumaAux' x y:xs)

-- 4) multiplicar 2 fracciones
mult4' :: Fraccion' -> Fraccion' -> Fraccion'
mult4' (F1 a b) (F1 c d) = F1 (a * c) (b * d)
mult4' (F1 a b) (F2 c)   = F1 (a * c) b
mult4' (F2 a) (F1 c d)   = F1 (a * c) d
mult4' (F2 a) (F2 b)     = F2 (a * b)

-- 5) restar 2 fracciones
resta5' :: Fraccion' -> Fraccion' -> Fraccion'
resta5' (F1 a b) (F1 c d) = F1 (a * d - b * c) (b * d)
resta5' (F1 a b) (F2 c)   = F1 (a - (b * c)) b
resta5' (F2 a) (F1 c d)   = F1 ((a * d) - c) d
resta5' (F2 a) (F2 c)     = F2 (a - c)
-- 6) restar y multiplicar varias fracciones (poniendo paréntesis
    -- desde izquierda y desde derecha)
-- 7) convertir a cadena una fracción
cadena7' :: Fraccion' -> String
cadena7' (F1 a b)
        | b == 1 = show a
        | otherwise = show a ++ "/" ++ show b
cadena7' (F2 a) = show a

-- 8) convertir a cadena una lista de fracciones
cadena8' :: [Fraccion'] -> String
cadena8' [] = ""
cadena8' [x] = cadena7' x
cadena8' (x:xs) = cadena7' x ++ ", " ++ cadena8' xs

-- 9) buscar entre varias fracciones las que son positivas, negativas y nulas
esPositiva' :: Fraccion' -> Bool
esPositiva' (F1 a b) = not (signum a == (-1) || signum b == (-1))
esPositiva' (F2 a) = signum a /= (-1)

esNula' :: Fraccion' -> Bool
esNula' (F1 a b)
    | a == 0 = True
    | otherwise = False

buscar9' :: String -> [Fraccion'] -> [Fraccion']
buscar9' _ [] = []
buscar9' arg (x:xs)
    | arg == "pos" = if esPositiva' x then x : buscar9' arg xs else buscar9' arg xs
    | arg == "neg" = if not (esPositiva' x) then x : buscar9' arg xs else buscar9' arg xs
    | arg == "null" = if esNula' x then x : buscar9' arg xs else buscar9' arg xs

-- 10) quitar de entre varias fracciones las equivalentes a una inicial
quitar10' :: Fraccion' -> [Fraccion'] -> [Fraccion']
quitar10' _ [] = []
quitar10' x (y:ys)
    | comp2' x y == "EQ" = quitar10' x ys
    | otherwise = y : quitar10' x ys

-- 11) ordenar en modo creciente una lista de fracciones
ordenar11' :: [Fraccion'] -> [Fraccion']
ordenar11' [] = []
ordenar11' [x] = [x]
ordenar11' (x:y:xs)
    | comp2' x y == "GT" = y : ordenar11' (x:xs)
    | otherwise = x : ordenar11' (y:xs)

-- 12) obtener todas las sumas posibles a partir de 2 listas de fracciones
    -- considerando una fracción de la primera lista y otra fracción de la
    -- segunda

aux :: Fraccion' -> [Fraccion'] -> [Fraccion']
aux _ [] = []
aux n [x] = [sumaAux' n x]
aux n (x:xs) = sumaAux' n x : aux n xs

--sumaListas12' :: [Fraccion'] -> [Fraccion'] -> [Fraccion']
sumaListas12' [] [] = []
sumaListas12' [x] [y] = [sumaAux' x y]
sumaListas12' [x] (y:ys) = aux x (y:ys)
sumaListas12' (x:xs) (y:ys) =  sumaListas12' (x:xs) (y:ys)

-- 13) obtener todas las fracciones equivalentes a partir de una inicial
equivalentes13' :: Fraccion' -> [Fraccion'] -> [Fraccion']
equivalentes13' _ [] = []
equivalentes13' x (y:xs)
    | comp2' x y == "EQ" = y : equivalentes13' x xs
    | otherwise = equivalentes13' x xs

-- 14) simplificar al máximo los elementos de una lista de fracciones
-- y eliminar las equivalentes.

quitarDups :: [Fraccion'] -> [Fraccion']
quitarDups [] = []
quitarDups [x] = [x]
quitarDups (x:y:xs)
    | comp2' x y == "EQ" = quitarDups (x:xs)
    | otherwise = y : quitarDups (y:xs)

simplificar14' :: [Fraccion'] -> [Fraccion']
simplificar14' [] = []
simplificar14' (x:xs) = simp1' x : simplificar14' xs

frac1', frac2', frac3', frac4' :: Fraccion'
frac1' = F1 1 5
frac2' = F1 9 7
frac3' = F1 540 420
frac4' = F1 4 20

lista' :: [Fraccion']
lista' = [frac1', frac2', frac3', frac4']

frac1'', frac2'', frac3'' :: Fraccion'
frac1'' = F2 2
frac2'' = F2 4
frac3'' = F2 6

lista'' :: [Fraccion']
lista'' = [frac2'', frac1'', frac3'']

frac1''' :: Fraccion'
frac1''' = 3 :/ 2

main :: IO ()
main = do
    putStrLn "\n\n ------------- Apartado b) ------------- \n"
    -- putStrLn "Simplificador de fracciones:"
    -- print $ simp1' frac2'
    -- putStrLn "\nComparación de fracciones:"
    -- print $ comp2' frac1' frac2'
    -- putStrLn "\nSuma de una lista de fracciones:"
    -- print $ suma3' lista'
    -- putStrLn "\nMult. de 2 fracciones:"
    -- print $ mult4' frac1' frac1''
    -- putStrLn "\nResta de 2 fracciones:"
    -- print $ resta5' frac1'' frac2''
    -- putStrLn "\nConvertir a cadena una fracción:"
    -- print $ cadena7' frac1'
    -- putStrLn "\nConvertir a cadena una lista fracciones:"
    -- print $ cadena8' lista'
    -- putStrLn "\nBuscar entre varias fracciones las que son positivas, negativas y nulas:"
    -- print $ buscar9' "pos" lista'
    -- print $ buscar9' "neg" lista'
    -- print $ buscar9' "null" lista'
    -- putStrLn "\nQuitar una fracción de una lista:"
    -- print $ quitar10' frac1' lista'
    -- putStrLn "\nOrdenar una lista de manera ascendente:"
    -- print lista''
    -- print $ ordenar11' lista''
    -- putStrLn "\nObtener todas las fracciones equivalentes a partir de una inicial:"
    putStrLn "\nObtener todas las sumas posibles de 2 listas"
    print $ aux (F1 1 1) [F1 2 1, F1 2 1]
    print $ sumaListas12' [F1 1 1, F1 2 1] [F1 2 1, F1 2 1]
    -- print $ equivalentes13' frac1' lista'
    -- putStrLn "\nsimplificar al máximo los elementos de una lista de fracciones\ny eliminar las equivalentes: "
    -- print lista'
    -- print $ simplificar14' lista'

-- [F1a 1 1, F1b 1 1]
-- [F1c 2 1, F1d 2 1]
-- 
-- F1a + F1c = 3
-- F1a + F1d = 3
-- F1b + F1c = 3
-- F1b + F1d = 3