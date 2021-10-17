module MainApartadoD (mainApartadoD) where

import           Func.Definiciones

{-
    d) Repetir el apartado b) usando una consola y stack, y creando el proyecto
    practica1a basado en el resolver lts-8.18. El proyecto se crea mediante la
    instrucción: “stack --resolver lts-8.18 new practica1a”, se compila mediante
    stack build y se ejecuta mediante stack run dentro de la carpeta practica1a
-}

mainApartadoD :: IO ()
mainApartadoD = do
    print $ coger 1 [3,4,3,5]
    print $ coger 2 [3,4,3,5]
    print $ coger 3 [3,4,3,5]
    -- print $ repetir [3,4,3]
