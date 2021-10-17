module MainApartadoI
    ( mainApartadoI ) where

{---------------------------------------------------------------------
    e) A partir del apartado c) crear una función que asigne para
    cada número entero positivo una fracción, de manera que 2
    números distintos obtengan fracciones no equivalentes y además
    cada fracción provenga exactamente de un número entero positivo.
    Estaríamos probando que el conjunto de los números racionales
    (Q) es numerable.
----------------------------------------------------------------------}
data Fraccion =
    F1 Integer Integer |
    F2 Integer         |
    Integer :/ Integer
    deriving Show

simp1 :: Fraccion -> Fraccion
simp1 (F1 a b) = F1 ((signum b * a) `div` m) (abs b `div` m)
    where m = gcd a b
simp1 (F2 a) = F2 a
simp1 (a :/ b)
    | b == 1 = F2 ((signum b * a) `div` m)
    | otherwise = ((signum b * a) `div` m) :/ (abs b `div` m)
    where m = gcd a b

comp2 :: Fraccion -> Fraccion -> String
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

comp2 (a :/ b) (c :/ d)
    | y > 0 = "GT"
    | y < 0 = "LT"
    | otherwise = "EQ"
    where y = (a * d) - (b * c)
comp2 (a :/ b) (F2 c)
    | y > 0 = "GT"
    | y < 0 = "LT"
    | otherwise = "EQ"
    where y = a - (b * c)
comp2 (F2 a) (c :/ d)
    | y > 0 = "GT"
    | y < 0 = "LT"
    | otherwise = "EQ"
    where y = (a * d) - c
comp2 (a :/ b) (F1 c d)
    | y > 0 = "GT"
    | y < 0 = "LT"
    | otherwise = "EQ"
    where y = (a * d) - (b * c)
comp2 (F1 a b) (c :/ d)
    | y > 0 = "GT"
    | y < 0 = "LT"
    | otherwise = "EQ"
    where y = (a * d) - (b * c)

ordenar11 :: [Fraccion] -> [Fraccion]
ordenar11 [] = []
ordenar11 (x:xs) =
    let primeraMitad = ordenar11 [ f1 | f1 <- xs, comp2 f1 x == "LT" || comp2 f1 x == "EQ"]
        segundaMitad = ordenar11 [ f1 | f1 <- xs, comp2 f1 x == "GT"]
    in  primeraMitad ++ [x] ++ segundaMitad


equivalentes13 :: Fraccion -> [Fraccion] -> [Fraccion]
equivalentes13 f1 xs = [x | x <- xs, comp2 f1 x == "EQ"]

quitarDups' :: [Fraccion] -> [Fraccion]
quitarDups' [] = []
quitarDups' [x] = [x]
quitarDups' (x:y:xs)
    | comp2 x y == "EQ" = quitarDups' (x:xs)
    | otherwise = x : quitarDups' (y:xs)

simplificarAux :: [Fraccion] -> [Fraccion]
simplificarAux xs = [ simp1 x | x <- xs ]

simplificar14 :: [Fraccion] -> [Fraccion]
simplificar14 x = quitarDups' (ordenar11 (simplificarAux x))

frac1', frac2', frac3', frac4' :: Fraccion
frac1' = F1 1 5
frac2' = 9 :/ 7
frac3' = F1 540 420
frac4' = F1 4 20

lista' :: [Fraccion]
lista' = [frac1', frac2', frac3', frac4']

georgeCantor' xs = xs

georgeCantor n = georgeCantor' [1..n]

mainApartadoI :: IO ()
mainApartadoI = print $ georgeCantor 10

{---------------------------------------------------------------------
    RESULTADO DEL PROGRAMA

[9 :/ 2,F1 12 1,11 :/ 2,F2 13]
---------------------------------------------------------------------}
