{- PRACTICA 2 Aplicación Parcial y Composición -}

{- 1 -}
siguiente :: Int -> Int
siguiente = (+ 1)

{- 2 -}
mitad :: Float -> Float
mitad = (/ 2)

{- 3 -}
inversa :: Float -> Float
inversa = (recip)

{- 4 -}
triple :: Int -> Int
triple = (* 3)

{- 5 -}
esNumeroPositivo :: Int -> Bool
esNumeroPositivo = (>= 0)

{- 6 -}
esMultiploDeV2 :: Int -> Int -> Bool
esMultiploDeV2 dividendo divisor = ((== 0) . (mod dividendo)) divisor

{- 7
esBisiestoV2 :: Int -> Bool
esBisiestoV2 anio = ((esMultiploDeV2 4) . (not (esMultiploDeV2 100))) anio
-}

{- 8 -}
inversaRaizCuadrada :: Float -> Float
inversaRaizCuadrada = inversa . sqrt

{- 9 -}
incrementMCuadradoN :: Int -> Int -> Int
incrementMCuadradoN m n = ((+ n) . (^ 2)) m

{- 10 -}
esResultadoPar :: Int -> Int -> Bool
esResultadoPar n m = (even . (^ m)) n