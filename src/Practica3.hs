{- PRACTICA 3 Tuplas -}

{- 1 -}
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

{- 2 -}
aplicar :: (t -> a, t -> b) -> t -> (a, b)
aplicar (func1, func2) num = (func1 num, func2 num)

{- 3 -}
cuentaBizarra :: (Int, Int) -> Int
cuentaBizarra (num1, num2)
  | num1 > num2 = num1 + num2
  | (num2 - num1) > 10 = num2 - num1
  | (num2 > num1) && ((num2 - num1) < 10) = num1 * num2

{- 4 -}
esNotaBochazo :: Int -> Bool
esNotaBochazo num = num < 6

aprobo :: (Int, Int) -> Bool
aprobo (nota1, nota2) = not (esNotaBochazo nota1) && not (esNotaBochazo nota2)

promociono :: (Int, Int) -> Bool
promociono (nota1, nota2) = (nota1 + nota2 >= 15) && nota1 >= 7 && nota2 >= 7

aproboPrimerParcial :: (Int, Int) -> Bool
aproboPrimerParcial (nota1, nota2) = (not . esNotaBochazo) nota1

{- 5 -}
notasFinales :: ((Int, Int), (Int, Int)) -> (Int, Int)
notasFinales ((parc1, parc2), (recup1, recup2)) = (max parc1 recup1, max parc2 recup2)

recursa :: ((Int, Int), (Int, Int)) -> Bool
recursa ((parc1, parc2), (recup1, recup2)) =
  ((not . aprobo) . notasFinales) ((parc1, parc2), (recup1, recup2))

recuperoPrimerParcial :: ((Int, Int), (Int, Int)) -> Bool
recuperoPrimerParcial ((parc1, parc2), (recup1, recup2)) =
  ((not . aproboPrimerParcial)) (parc1, parc2)

{- recuperoDeGusto :: ((Int, Int), (Int, Int)) -> Bool
recuperoDeGusto ((parc1, parc2), (recup1, recup2)) =
  FALTA TERMINAR
-}

{- 6 -}
esMayorDeEdad :: (String, Int) -> Bool
esMayorDeEdad (nombre, edad) = mayorDe21 edad

mayorDe21 :: Int -> Bool
mayorDe21 n = n > 21

{- 7 -}
calcular :: (Int, Int) -> (Int, Int)
calcular (num1, num2) = (hayQueDuplicar num1, hayQueSumarUno num2)

hayQueDuplicar :: Int -> Int
hayQueDuplicar num
  | even num = num * 2
  | otherwise = num

hayQueSumarUno :: Int -> Int
hayQueSumarUno num
  | odd num = num + 1
  | otherwise = num