{- PRACTICA 4 Orden Superior y Listas -}

{- Listas -}

{- 1 -}
sumarListaDeNumeros :: [Int] -> Int
sumarListaDeNumeros lista = sum lista

{- 2 -}
frecuenciaCardiaca :: [Int]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]

promedioFrecuenciaCardiaca :: Float
promedioFrecuenciaCardiaca =
  fromIntegral (sum frecuenciaCardiaca) / fromIntegral (length frecuenciaCardiaca)

numeroDeMuestra :: Int -> Int
numeroDeMuestra m = div m 10

frecuenciaCardiacaMinuto :: Int -> Int
frecuenciaCardiacaMinuto m = frecuenciaCardiaca !! numeroDeMuestra m

frecuenciasHastaMomento :: Int -> [Int]
frecuenciasHastaMomento m = take ((+ 1) . numeroDeMuestra $ m) frecuenciaCardiaca

{- 3 -}
esCapicua :: Eq a => [[a]] -> Bool
esCapicua listaDeListas = concat listaDeListas == reverse (concat listaDeListas)

{- 4 -}
duracionLlamadas :: ((String, [Int]), (String, [Int]))
duracionLlamadas =
  (("horarioReducido", [20, 10, 25, 15]), ("horarioNormal", [10, 5, 8, 2, 9, 10]))

cuandoHabloMasMinutos :: String
cuandoHabloMasMinutos
  | sum (snd . fst $ duracionLlamadas) > sum (snd . snd $ duracionLlamadas) = fst . fst $ duracionLlamadas
  | otherwise = fst . snd $ duracionLlamadas

cuandoHizoMasLlamadas :: String
cuandoHizoMasLlamadas
  | length (snd . fst $ duracionLlamadas) > length (snd . snd $ duracionLlamadas) = fst . fst $ duracionLlamadas
  | otherwise = fst . snd $ duracionLlamadas

{- Orden superior -}

{- 1 -}
existsAny :: (t -> Bool) -> (t, t, t) -> Bool
existsAny funcion (x, y, z) = funcion x || funcion y || funcion z

{- 2 -}
mejor :: Ord a => (t -> a) -> (t -> a) -> t -> a
mejor func1 func2 num = max (func1 num) (func2 num)

{- 3 -}
aplicarPar :: (a -> b) -> (a, a) -> (b, b)
aplicarPar f (x, y) = (f x, f y)

{- 4 -}
parDeFns :: (t -> a) -> (t -> b) -> t -> (a, b)
parDeFns f1 f2 x = (f1 x, f2 x)

{- Orden superior + Listas -}

{- 1 -}
esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno num lista = any (esMultiploDe num) lista

{- 2 -}
promedios :: [[Float]] -> [Float]
promedios xss = map (\lista -> sum lista / fromIntegral (length lista)) xss

{- 3 -}
promediosSinAplazos :: [[Float]] -> [Float]
promediosSinAplazos xss = map promedioSinAplazos xss

promedioSinAplazos :: [Float] -> Float
promedioSinAplazos xs = sum ys / fromIntegral (length ys)
  where
    ys = filter (>= 4) xs

{- 4 -}
mejoresNotas :: [[Int]] -> [Int]
mejoresNotas xss = map maximum xss

{- 5 -}
aproboV2 :: [Int] -> Bool
aproboV2 xs = (>= 6) . minimum $ xs

{- 6 -}
aprobaron :: [[Int]] -> [[Int]]
aprobaron curso = filter aproboV2 curso