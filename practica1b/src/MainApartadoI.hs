module MainApartadoI
    ( mainApartadoI ) where

{---------------------------------------------------------------------
    e) A partir del apartado c) crear una función que asigne para
    cada número entero positivo una fracción, de manera que 2
    números distintos obtengan fracciones no equivalentes y además
    cada fracción provenga exactamente de un número entero positivo.
    Estaríamos probando que el conjunto de los números racionales
    (Q) es numerable.

    El siguiente programa se realizó basándose en el siguiente
    código: https://www.geeksforgeeks.org/n-th-term-of-george-cantor-set-of-rational-numbers/
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

quitarDups' :: [Fraccion] -> [Fraccion]
quitarDups' [] = []
quitarDups' [x] = [x]
quitarDups' (x:y:xs)
    | comp2 x y == "EQ" = quitarDups' (x:xs)
    | otherwise = x : quitarDups' (y:xs)

simplificarAux :: [Fraccion] -> [Fraccion]
simplificarAux xs = [ simp1 x | x <- xs ]

diagonalAbajo n k (F1 a b) xs = if a > 1 && k < n then diagonalAbajo n (k + 1) (F1 (a - 1) (b + 1)) (F1 a b : xs) else reverse (quitarDups' (simplificarAux (F1 a b : xs)))

diagonalArribaAux n k (F1 a b) xs = if k /= n then diagonalAbajo n (k + 1) (F1 (a + 1) b) (F1 a b : xs) else F1 a b : xs

diagonalArriba n k (F1 a b) xs = if b > 1 && k < n then diagonalArribaAux n (k + 1) (F1 (a + 1) (b - 1)) (F1 a b : xs) else F1 a b : xs

georgeCantor' n k (F1 a b) xs = if k == n then F1 a b : xs else diagonalArriba n (k + 1) (F1 a (b + 1)) (F1 a b : xs)

racionalesNumerables n = georgeCantor' n 1 (F1 1 1) []

mainApartadoI :: IO ()
mainApartadoI = print $ racionalesNumerables 4

{---------------------------------------------------------------------
    RESULTADO DEL PROGRAMA

[F1 1 1,F1 1 2,F1 2 1,F1 3 1]
---------------------------------------------------------------------}
