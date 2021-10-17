module MainApartadoO (mainApartadoO) where

import           Debug.Trace
import           Func.Definiciones

{-
    n) Usar la función trace para la ejecución recursiva de la función coger
    sobre un ejemplo concreto, mostrando en pantalla los diferentes resultados
    de ejecución de la función coger. Además, explicar al estilo de la diapositiva
    16 del fichero 03-introduccion-hs.ppt, cuál sería la cadena de evaluaciones
    a partir de la expresión de partida.
-}

traceCoger :: Integer -> [Integer] -> [Integer]
traceCoger x xs = trace ("DEBUG coger " ++ show x ++" | " ++ show xs) coger x xs

mainApartadoO :: IO ()
mainApartadoO = print $ traceCoger 2 [0, 1, 2, 3, 4]
