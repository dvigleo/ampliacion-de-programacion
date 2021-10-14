module MainApartadoC
    (
        mainApartadoC
    ) where

{---------------------------------------------------------------------
    c) Usando listas por comprensión y funciones de orden superior
    mostrar cómo se podría codificar alternativamente y de un modo más
    compacto el apartado anterior.
----------------------------------------------------------------------}

data Fraccion' =
    F1 Integer Integer |
    F2 Integer         |
    Integer :/ Integer
    deriving Show

-- 1) simplificar al máximo una fracción
simp1 :: Fraccion' -> Fraccion'
simp1 (F1 a b) = F1 ((signum b * a) `div` m) (abs b `div` m)
    where m = gcd a b
simp1 (F2 a) = F2 a

-- 2) comparar 2 fracciones
comp2 :: Fraccion' -> Fraccion' -> String
comp2 (F1 a b) (F1 c d)
    | y > 0 = "GT"
    | y < 0 = "LT"
    | otherwise = "EQ"
    where y = (a * d) - (b * c)
comp2 (F1 a b) (F2 c)
    | y > 0 = "GT"
    | y < 0 = "LT"
    | otherwise = "EQ"
    where y = a - (b * c)
comp2 (F2 a) (F1 c d)
    | y > 0 = "GT"
    | y < 0 = "LT"
    | otherwise = "EQ"
    where y = (a * d) - c
comp2 (F2 a) (F2 b)
    | y > 0 = "GT"
    | y < 0 = "LT"
    | otherwise = "EQ"
    where y = a - b

{-
    3) sumar varias fracciones
        - suma3' = Función principal. Hace uso de la función foldl para tomar todos 
            los elementos de la lista por la izquierda y aplicarles la función de sumaAux
        - sumaAux' = Función que recibe dos fracciones y las suma de acuerdo a la fórmula matemática.
-}
sumaAux :: Fraccion' -> Fraccion' -> Fraccion'
sumaAux (F1 a b) (F1 c d) = simp1 (F1 (a * (y `div` b) + c * (y `div` d)) y)
    where y = (b * d) `div` gcd b d
sumaAux (F1 a b) (F2 c) = simp1 (F1 (a * (y `div` b) + c * y) y)
    where y = b `div` gcd b 1
sumaAux (F2 a) (F1 c d) = simp1 (F1 (a * y + c * (y `div` d)) y)
    where y = d `div` gcd 1 d
sumaAux (F2 a) (F2 b) = F2 (a + b)

suma3 :: [Fraccion'] -> Fraccion'
suma3 xs = foldl sumaAux (F1 0 1) xs

-- 4) multiplicar 2 fracciones
mult4 :: Fraccion' -> Fraccion' -> Fraccion'
mult4 (F1 a b) (F1 c d) = F1 (a * c) (b * d)
mult4 (F1 a b) (F2 c)   = F1 (a * c) b
mult4 (F2 a) (F1 c d)   = F1 (a * c) d
mult4 (F2 a) (F2 b)     = F2 (a * b)

-- 5) restar 2 fracciones
resta5 :: Fraccion' -> Fraccion' -> Fraccion'
resta5 (F1 a b) (F1 c d) = F1 (a * d - b * c) (b * d)
resta5 (F1 a b) (F2 c)   = F1 (a - (b * c)) b
resta5 (F2 a) (F1 c d)   = F1 ((a * d) - c) d
resta5 (F2 a) (F2 c)     = F2 (a - c)

{-
    6) restar y multiplicar varias fracciones (poniendo paréntesis desde izquierda y desde derecha)
-}
restarIzquierda6 :: [Fraccion'] -> Fraccion'
restarIzquierda6 = aux foldl resta5 (F1 0 1)

multIzquierda6 :: [Fraccion'] -> Fraccion'
multIzquierda6 = aux foldl mult4 (F1 1 1)

restarDerecha6 :: [Fraccion'] -> Fraccion'
restarDerecha6 = aux foldr resta5 (F1 0 1)

multDerecha6 :: [Fraccion'] -> Fraccion'
multDerecha6 = aux foldr mult4 (F1 1 1)

aux func operacion acc xs = func operacion acc xs

-- 7) convertir a cadena una fracción
cadena7 :: Fraccion' -> String
cadena7 (F1 a b)
        | b == 1 = show a
        | otherwise = show a ++ "/" ++ show b
cadena7 (F2 a) = show a

-- 8) convertir a cadena una lista de fracciones
cadena8 :: [Fraccion'] -> String
cadena8 []     = ""
cadena8 [x]    = cadena7 x
cadena8 (x:xs) = cadena7 x ++ ", " ++ cadena8 xs

{-
    9) buscar entre varias fracciones las que son positivas, negativas y nulas
        - buscar9 = Función de entrada. Recibe un argumento (arg) que puede ser 
            pos | neg | null para indicar qué clase de filtrado se hará sobre la lista
        - esPositiva = Función que regresa un booleano que indica si una fracción dada 
            es positiva o no. Revisa el símbolo del numerador o el denomindos según se requiera
        - filterNeg = Función que hace un filter de lo contrario a lo que se busca filtera; 
            en este caso es para encontrar los que NO son positivos
        - esNula = Función que regresa un booleano que indica si una fracción 
            dada es nula o no (si el numerador == 0)
-}
esPositiva :: Fraccion' -> Bool
esPositiva (F1 a b) = not (signum a == (-1) || signum b == (-1))
esPositiva (F2 a)   = signum a /= (-1)

filterNeg :: (a -> Bool) -> [a] -> [a]
filterNeg f = filter (not . f)

esNula :: Fraccion' -> Bool
esNula (F1 a b)
    | a == 0 = True
    | otherwise = False
esNula (F2 a)
    | a == 0 = True
    | otherwise = False

buscar9 :: String -> [Fraccion'] -> [Fraccion']
buscar9 _ [] = []
buscar9 arg xs
    | arg == "pos" = filter esPositiva xs
    | arg == "neg" = filterNeg esPositiva xs
    | arg == "null" = filter esNula xs
    | otherwise = error "Ese filtro no existe"

-- 10) quitar de entre varias fracciones las equivalentes a una inicial
quitar10 :: Fraccion' -> [Fraccion'] -> [Fraccion']
quitar10 x ys = [ f1 | f1 <- ys, comp2 f1 x /= "EQ"]

{-
    11) ordenar en modo creciente una lista de fracciones
        Se hace uso del agoritmoo "quick sort" para ordenar los elementos.
        Se toma el primer elemento de la lista y se coloca a la mitad de la lista (pivot)
            Los elementos a la izquierda y a la derecha del "pivot" se ordenan
-}

ordenar11 :: [Fraccion'] -> [Fraccion']
ordenar11 [] = []
ordenar11 (x:xs) =
    let primeraMitad = ordenar11 [ f1 | f1 <- xs, comp2 f1 x == "LT" || comp2 f1 x == "EQ"]  
        segundaMitad = ordenar11 [ f1 | f1 <- xs, comp2 f1 x == "GT"]  
    in  primeraMitad ++ [x] ++ segundaMitad  

{-
    12) obtener todas las sumas posibles a partir de 2 listas de fracciones
        considerando una fracción de la primera lista y otra fracción de la
        segunda
        - sumaListas12 = Recibe ambas listas obtiene todas las sumas 
            posibles usando una lista de comprensión
-}

sumaListas12 :: [Fraccion'] -> [Fraccion'] -> [Fraccion']
sumaListas12 xs ys = [sumaAux x y | x <- xs, y <- ys]

-- 13) obtener todas las fracciones equivalentes a partir de una inicial
equivalentes13 :: Fraccion' -> [Fraccion'] -> [Fraccion']
equivalentes13 f1 xs = [x | x <- xs, comp2 f1 x == "EQ"]

{-
    14) simplificar al máximo los elementos de una lista de fracciones y eliminar las equivalentes.
        - simplificar14' = Función de entrada. Se simplifica, ordena y eliminan duplicados
        - simplificarAux = Función que recibe una lista y regresa una lista con 
            todos sus elementos simplificados
        - quitarDups = Función que compara el primer elemento con el segundo y elimina
            el segundo si son iguales
-}
quitarDups' :: [Fraccion'] -> [Fraccion']
quitarDups' [] = []
quitarDups' [x] = [x]
quitarDups' (x:y:xs)
    | comp2 x y == "EQ" = quitarDups' (x:xs)
    | otherwise = x : quitarDups' (y:xs)

simplificarAux :: [Fraccion'] -> [Fraccion']
simplificarAux xs = [ simp1 x | x <- xs ]

simplificar14 :: [Fraccion'] -> [Fraccion']
simplificar14 x = quitarDups' (ordenar11 (simplificarAux x))

frac1', frac2', frac3', frac4' :: Fraccion'
frac1' = F1 1 5
frac2' = F1 9 7
frac3' = F1 540 420
frac4' = F1 4 20

lista', lista2' :: [Fraccion']
lista' = [frac1', frac2', frac3', frac4'] -- 13 y 14
lista2' = [F1 8 1, F1 2 1, F1 1 1, F2 (-2), F1 0 2] -- 9
lista3' = [F1 4 1, F2 5]
lista4' = [F1 1 2, F2 8]

frac1'', frac2'', frac3'' :: Fraccion'
frac1'' = F2 2
frac2'' = F2 4
frac3'' = F2 6

lista'' :: [Fraccion']
lista'' = [frac2'', frac1'', frac3'']

frac1''' :: Fraccion'
frac1''' = 3 :/ 2

mainApartadoC :: IO ()
mainApartadoC = do
    putStrLn "Simplificador de fracciones:"
    print $ "Original: " ++ cadena7 frac3'
    print $ cadena7 (simp1 frac3')

    putStrLn "\nComparación de fracciones:\n   | LT = Less Than\n   | GT = Greater Than\n   | EQ = Equal To\n"
    print $ cadena7 frac1' ++ " es " ++ comp2 frac1' frac2' ++ " " ++ cadena7 frac2'
    print $ cadena7 frac3'++ " es " ++ comp2 frac3' frac4' ++ " " ++  cadena7 frac4'
    print $ cadena7 frac1'++ " es " ++ comp2 frac1' frac4' ++ " " ++ cadena7 frac4'

    putStrLn "\nSuma de una lista de fracciones:"
    print $ "[ " ++ cadena8 lista2' ++ " ] = " ++ cadena7 (suma3 lista2')

    putStrLn "\nMult. de 2 fracciones:"
    print $ cadena7 frac1' ++ " x " ++ cadena7 frac1'' ++ " = " ++ cadena7 (mult4 frac1' frac1'')

    putStrLn "\nResta de 2 fracciones:"
    print $ cadena7 frac1'' ++ " - " ++ cadena7 frac2'' ++ " = " ++ cadena7 (resta5 frac1'' frac2'')

    putStrLn "\nRestar y multiplicar varias fracciones, poniendo paréntesis desde\nla izquierda y derecha:"
    print $ "[ " ++ cadena8 lista2' ++ " ]"
    putStrLn "\nRestando por la izquierda:"
    print $ restarIzquierda6 lista2'
    putStrLn "Restando por la derecha:"
    print $ restarDerecha6 lista2'
    putStrLn "Multiplicando por la izquierda:"
    print $ multIzquierda6 lista2'
    putStrLn "Multiplicando por la derecha:"
    print $ multDerecha6 lista2'

    putStrLn "\nConvertir a cadena una fracción:"
    print $ cadena7 frac1'

    putStrLn "\nConvertir a cadena una lista fracciones:"
    print $ cadena8 lista'

    putStrLn "\nBuscar entre varias fracciones las que son positivas, negativas y nulas:"
    print $ "[ " ++ cadena8 lista2' ++ " ]"
    print $ "Positivas: [ " ++ cadena8 (buscar9 "pos" lista2') ++ " ]"
    print $ "Negativas: [ " ++ cadena8 (buscar9 "neg" lista2') ++ " ]"
    print $ "Nulas: [ " ++ cadena8 (buscar9 "null" lista2') ++ " ]"

    putStrLn "\nQuitar una fracción de una lista:"
    print $ cadena7 frac1' ++ "  |  [" ++ cadena8 lista' ++ " ]"
    print $ cadena8 (quitar10 frac1' lista')

    putStrLn "\nOrdenar una lista de manera ascendente:"
    print $ "[ " ++ cadena8 lista' ++ " ] --> [ " ++ cadena8 (ordenar11 lista') ++ " ]"

    putStrLn "\nObtener todas las sumas posibles de 2 listas"
    print $ "[ " ++ cadena8 lista3' ++ " ] X [ " ++ cadena8 lista4' ++ " ]"
    print $ "[ " ++ cadena8 (sumaListas12 lista3' lista4') ++ " ]"

    putStrLn "\nObtener todas las fracciones equivalentes a partir de una inicial:"
    print $ cadena7 frac1' ++ "  |  [" ++ cadena8 lista' ++ " ]"
    print $ "[ " ++ cadena8 (equivalentes13 frac1' lista') ++ " ]"

    putStrLn "\nSimplificar al máximo los elementos de una lista de fracciones y eliminar las equivalentes: "
    print $ "[ " ++ cadena8 lista' ++ " ] --> [ " ++ cadena8(simplificar14 lista') ++ " ]"
