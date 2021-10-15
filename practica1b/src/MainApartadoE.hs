{-# LANGUAGE GADTs #-}

module MainApartadoE
    (
        mainApartadoE
    ) where

{---------------------------------------------------------------------
    e) A partir del apartado anterior, utilizar el algoritmo de
    ordenación de la diapositiva 4 del fichero 02-introduccion-pf.ppt
    para ordenar crecientemente una lista de fracciones,
    haciendo que el tipo (Fraccion a) sea una instancia de la clase Ord.

----------------------------------------------------------------------}

-- data Fraccion a where
--     F1 :: Integral a => a -> a -> Fraccion a
--     F2 :: Integral a => a -> Fraccion a

data Fraccion = F Integer deriving (Show, Eq)

-- instance Ord Fraccion where
--      compare x y = if x == y then EQ else if x < y then LT else GT
    --(F x) (==) (F y) = F x == F y

-- instance Ord a => Ord (Fraccion a)
lista = [F 8, F 2, F 1, F (-2), F 0]

quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x]
                                            ++ [x] ++
                                                quicksort [z | z <- xs, z > x]

mainApartadoE :: IO ()
mainApartadoE = do
    print $ quicksort [1, 4, 5, 2, 0, 1, 5]
   -- print lista

