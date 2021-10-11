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

{-
    3) sumar varias fracciones
        - suma3' = Función principal. Recorre todos los elementos de la lista. Llama una función auxiliar para ir haciendo las sumas correspondientes
        - sumaAux' = Función que recibe dos fracciones y las suma de acuerdo a la fórmula matemática. 
-} 
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
restarIzquierda6' :: [Fraccion'] -> [Fraccion']
restarIzquierda6' [] = []
restarIzquierda6' [x] = [x]
restarIzquierda6' (x:y:xs) = restarIzquierda6' ((resta5' x y) : xs)

multIzquierda6' :: [Fraccion'] -> [Fraccion']
multIzquierda6' [] = []
multIzquierda6' [x] = [x]
multIzquierda6' (x:y:xs) = multIzquierda6' ((mult4' x y) : xs)

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

{-
    9) buscar entre varias fracciones las que son positivas, negativas y nulas
        - buscar9' = Función de entrada. Recibe un argumento (arg) que puede ser pos | neg | null para indicar qué clase de filtrado se hará sobre la lista
        - esPositiva' = Función que regresa un booleano que indica si una fracción dada es positiva o no. Revisa el símbolo del numerador o el denomindos según se requiera
        - esNula' = Función que regresa un booleano que indica si una fracción dada es nula o no (si el numerador == 0)
-}
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
    | otherwise = error "Ese filtro no existe"

-- 10) quitar de entre varias fracciones las equivalentes a una inicial
quitar10' :: Fraccion' -> [Fraccion'] -> [Fraccion']
quitar10' _ [] = []
quitar10' x (y:ys)
    | comp2' x y == "EQ" = quitar10' x ys
    | otherwise = y : quitar10' x ys

{-
    11) ordenar en modo creciente una lista de fracciones
        - ordenar11' Función de entrada, se pasa la lista y un valor de 0 para saber cuántas veces se debe de iterar sobre la lista
        - bubblesort = Función para iterar N cantidad de veces sobre la lista; donde N = tamaño de la lista
        - bubblesortAux = Función de ordenamiento; compara el primer elemento con el segundo y los intercambia si el segundo es menor
-} 
bubblesortAux :: [Fraccion'] -> [Fraccion']
bubblesortAux [] = []
bubblesortAux [x] = [x]
bubblesortAux (x:y:xs)
    | comp2' x y == "GT" = y : bubblesortAux (x:xs)
    | otherwise = x : bubblesortAux (y:xs)

bubblesort :: [Fraccion'] -> Int -> [Fraccion']
bubblesort xs i
    | i == length xs = xs
    | otherwise = bubblesort (bubblesortAux xs) (i + 1)

ordenar11' :: [Fraccion'] -> [Fraccion']
ordenar11' x = bubblesort x 0

{-
    12) obtener todas las sumas posibles a partir de 2 listas de fracciones
        considerando una fracción de la primera lista y otra fracción de la
        segunda
        - sumaListas12' = Función de entrada. Recibe ambas listas obtiene las sumas y regresa una lista con todas las sumas posibles
        - sumarNconLista = Función que se utiliza para sumar un número con todos los elementos de una lista
-}

sumarNconLista :: Fraccion' -> [Fraccion'] -> [Fraccion']
sumarNconLista _ [] = []
sumarNconLista n [x] = [sumaAux' n x]
sumarNconLista n (x:xs) = sumaAux' n x : sumarNconLista n xs

sumaListas12' :: [Fraccion'] -> [Fraccion'] -> [Fraccion']
sumaListas12' [] [] = []
sumaListas12' [x] [y] = [sumaAux' x y]
sumaListas12' [x] (y:ys) = sumarNconLista x (y:ys)
sumaListas12' (x:xs) (y:ys) =  sumarNconLista x (y:ys) ++ sumaListas12' xs (y:ys)

-- 13) obtener todas las fracciones equivalentes a partir de una inicial
equivalentes13' :: Fraccion' -> [Fraccion'] -> [Fraccion']
equivalentes13' _ [] = []
equivalentes13' x (y:xs)
    | comp2' x y == "EQ" = y : equivalentes13' x xs
    | otherwise = equivalentes13' x xs

{-
    14) simplificar al máximo los elementos de una lista de fracciones y eliminar las equivalentes.
        - simplificar14' = Función de entrada. Se simplifica, ordena y eliminan duplicados
        - simplificarAux = Función que recibe una lista y regresa una lista con todos sus elementos simplificados
        - quitarDups = Función que compara el primer elemento con el segundo y elimina el segundo si son iguales
-}

quitarDups :: [Fraccion'] -> [Fraccion']
quitarDups [] = []
quitarDups [x] = [x]
quitarDups (x:y:xs)
    | comp2' x y == "EQ" = quitarDups (x:xs)
    | otherwise = x : quitarDups (y:xs)

simplificarAux' :: [Fraccion'] -> [Fraccion']
simplificarAux' [] = []
simplificarAux' (x:xs) = simp1' x : simplificar14' xs

simplificar14' :: [Fraccion'] -> [Fraccion']
simplificar14' x = quitarDups (ordenar11' (simplificarAux' x))

frac1', frac2', frac3', frac4' :: Fraccion'
frac1' = F1 1 5
frac2' = F1 9 7
frac3' = F1 540 420
frac4' = F1 4 20

lista', lista2' :: [Fraccion']
-- lista' = [frac1', frac2', frac3', frac4']
lista' = [F1 8 1, F1 2 1, F1 1 1, F2 2]
lista2' = [F1 4 1, F1 5 1]

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

    putStrLn "\nRestar y multiplicar varias fracciones, poniendo paréntesis desde\nla izquierda y derecha:"
    print $ restarIzquierda6' lista'
    print $ multIzquierda6' lista'

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
    -- print lista'
    -- print $ ordenar11' lista'

    -- putStrLn "\nObtener todas las sumas posibles de 2 listas"
    -- print lista'
    -- print lista2'
    -- print $ sumaListas12' lista' lista2'

    -- putStrLn "\nObtener todas las fracciones equivalentes a partir de una inicial:"
    -- print $ "Fraccion inicial: " ++ cadena7' frac1'
    -- print $ equivalentes13' frac1' lista'
    
    -- putStrLn "\nSimplificar al máximo los elementos de una lista de fracciones\ny eliminar las equivalentes: "
    -- print lista'
    -- print $ quitarDups (ordenar11' (simplificar14' lista'))