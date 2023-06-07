{-# LANGUAGE NoDefaultSignatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

import Data.List

{- PRACTICA 1 Básicos-}

{- 1 -}
esMultiploDeTres :: Int -> Bool
esMultiploDeTres num = mod num 3 == 0

{- 2 -}
esMultiploDe :: Int -> Int -> Bool
esMultiploDe multiplo divisor = mod multiplo divisor == 0

{- 3 -}
cubo :: Int -> Int
cubo num = num ^ 3

{- 4 -}
area :: Int -> Int -> Int
area base altura = base * altura

{- 5 -}
esBisiesto :: Int -> Bool
esBisiesto anio = esMultiploDe anio 4 && not (esMultiploDe anio 100)

{- 6 -}
celciusToFahr :: Float -> Float
celciusToFahr celcius = celcius * 1.8 + 32

{- 7 -}
fahrToCelsius :: Float -> Float
fahrToCelsius fahr = (fahr - 32) * 5 / 9

{- 8 -}
haceFrio :: Float -> Bool
haceFrio fahr = fahrToCelsius fahr < 8

{- 9 -}
mcm :: Int -> Int -> Int
mcm num1 num2 = gcd num1 num2

{- 10 -}
dispersion :: Int -> Int -> Int -> Int
dispersion dia1 dia2 dia3 =
  (max (max dia1 dia2) dia3) - (min (min dia1 dia2) dia3)

diasParejos :: Int -> Int -> Int -> Bool
diasParejos dia1 dia2 dia3 = dispersion dia1 dia2 dia3 < 30

diasLocos :: Int -> Int -> Int -> Bool
diasLocos dia1 dia2 dia3 = dispersion dia1 dia2 dia3 > 100

diasNormales :: Int -> Int -> Int -> Bool
diasNormales dia1 dia2 dia3 =
  not (diasParejos dia1 dia2 dia3) && not (diasLocos dia1 dia2 dia3)

{- 11 -}
pesoPino :: Int -> Int
pesoPino altura
  | altura <= 300 = altura * 3
  | otherwise = (altura - 300) * 2 + 300 * 3

esPesoUtil :: Int -> Bool
esPesoUtil peso = peso > 400 && peso < 1000

sirvePino :: Int -> Bool
sirvePino altura = esPesoUtil (pesoPino altura)

{- 12 SIN HACER-}
{- esCuadradoPerfecto n = sumarImpares (n 0 1) == n -}

{-
sumarImpares n i j
  | (n == 0) || (n < i) = 0
  | (n == i) = n
  | n > i = sumarImpares n (i + j) (j + 2) -}