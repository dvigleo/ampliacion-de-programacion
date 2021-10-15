module MainApartadoE
    ( mainApartadoE ) where

{---------------------------------------------------------------------
    e) A partir del apartado anterior, utilizar el algoritmo de
    ordenación de la diapositiva 4 del fichero 02-introduccion-pf.ppt
    para ordenar crecientemente una lista de fracciones,
    haciendo que el tipo (Fraccion a) sea una instancia de la clase Ord.
----------------------------------------------------------------------}

data (Integral a) => Fraccion a =
    F1 a a | F2 a | a :/ a
    deriving (Show, Eq)

instance (Integral a) => Ord (Fraccion a) where
    (x:/y)   <= (x':/y')   = x * y' <= x' * y
    (x:/y)   <= (F1 x' y') = x * y' <= x' * y
    (x:/y)   <= (F2 x')    =     x  <= x' * y

    (F1 x y) <= (F1 x' y') = x * y' <= x' * y
    (F1 x y) <= (x':/y')   = x * y' <= x' * y
    (F1 x y) <= (F2 x')    =     x  <= x' * y

    (F2 x)   <= (F2 x')    =     x  <= x'
    (F2 x)   <= (x':/y')   = x * y' <= x'
    (F2 x)   <= (F1 x' y') = x * y' <= x'

    (x:/y)   < (x':/y')   = x * y' <  x' * y
    (x:/y)   < (F1 x' y') = x * y' <  x' * y
    (x:/y)   < (F2 x')    = x <  x' * y

    (F1 x y) < (F1 x' y') = x * y' <  x' * y
    (F1 x y) < (x':/y')   = x * y' <  x' * y
    (F1 x y) < (F2 x')    =      x <  x' * y

    (F2 x)   < (x':/y')   = x * y' <  x'
    (F2 x)   < (F2 x')    = x <  x'
    (F2 x)   < (F1 x' y') = x * y' <  x'

lista :: [Fraccion Integer]
lista = [ F1 1 5, 9 :/ 7, F1 540 420, F1 4 20 ]

quicksort :: Ord t => [t] -> [t]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [z | z <- xs, z > x]

mainApartadoE :: IO ()
mainApartadoE = do
    print lista
    print $ quicksort lista
