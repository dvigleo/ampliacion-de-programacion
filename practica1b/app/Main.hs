module Main where

import           MainApartadoB
import           MainApartadoC
import           MainApartadoE
import           MainApartadoG

main :: IO ()
main = do
    putStrLn "\n\n ------------- Apartado b) ------------- \n"
    mainApartadoB
    putStrLn "\n\n ------------- Apartado c) ------------- \n"
    mainApartadoC
    putStrLn "\n\n ------------- Apartado e) ------------- \n"
    mainApartadoE
    putStrLn "\n\n ------------- Apartado g) ------------- \n"
    mainApartadoG
