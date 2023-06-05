------------
---Punto1---
------------

type Grito = (String, Int, Bool)

onomatopeya (o,_,_) = o
intensidad (_,i,_) = i
mojoLaCama (_,_,m) = m

energiaDeGrito :: Grito -> Int
energiaDeGrito unGrito
    | mojoLaCama unGrito = (* ((intensidad unGrito) ^ 2)) . nivelDeTerror $ unGrito
    | otherwise = (+3) . (3*) . nivelDeTerror $ unGrito

nivelDeTerror :: Grito -> Int
nivelDeTerror = length . onomatopeya

------------
---Punto2---
------------

type Ninio = (String, Int, Float)

nombre (n,_,_) = n
edad (_,e,_) = e
altura (_,_,a) = a

type Monstruo = Ninio -> Grito

sullivan :: Monstruo
sullivan unNinio = crearGrito (onomatopeyaSullivan unNinio) (div 20 (edad unNinio)) (siTieneMenosDe3 unNinio)

onomatopeyaSullivan :: Ninio -> String
onomatopeyaSullivan unNinio = (replicate (length . nombre $ unNinio) 'A') ++ "GH"

siTieneMenosDe3 :: Ninio -> Bool
siTieneMenosDe3 = (<3) . edad

randall :: Monstruo
randall unNinio = crearGrito "¡Mamadera!" (intensidadRandall unNinio) (mideEntre08Y12 unNinio)

intensidadRandall :: Ninio -> Int
intensidadRandall = vocalesEnElNombre . nombre

vocalesEnElNombre :: String -> Int
vocalesEnElNombre = length . filter esVocal

esVocal :: Char -> Bool
esVocal char = elem char "aeiou"

mideEntre08Y12 :: Ninio -> Bool
mideEntre08Y12 unNinio = altura unNinio > 0.8 && altura unNinio < 1.2

chuckNorris :: Monstruo
chuckNorris unNinio = crearGrito todoElAbecedario 1000 True

todoElAbecedario :: String
todoElAbecedario = ['a'..'z']

osito :: Monstruo
osito unNinio = crearGrito "uf" (edad unNinio) False

crearGrito :: String -> Int -> Bool -> Grito
crearGrito onomatopeya intensidad mojoLaCama = (onomatopeya, intensidad, mojoLaCama)

------------
---Punto3---
------------

pam :: [(a -> b)] -> a -> [b]
pam funciones elemento = map ($ elemento) funciones

------------
---Punto4---
------------

gritos :: [Monstruo] -> Ninio -> [Grito]
gritos = pam

------------
---Punto5---
------------

type Campamento = [Ninio]

produccionEnergeticaGritos :: [Monstruo] -> Campamento -> Int
produccionEnergeticaGritos equipoMonstruos = 
   sum . map energiaDeGrito . concatMap (gritos equipoMonstruos)

------------
---Punto6---
------------

type Risa = (Int, Int)

duracionRisa :: Risa -> Int
duracionRisa = fst

intensidadRisa :: Risa -> Int
intensidadRisa = snd

energiaDeRisa :: Risa -> Int
energiaDeRisa unaRisa = (duracionRisa unaRisa) ^ (intensidadRisa unaRisa)

risas :: [Comediante] -> Ninio -> [Risa]
risas = pam

type Comediante = Ninio -> Risa

produccionEnergeticaRisas :: [Comediante] -> Campamento -> Int
produccionEnergeticaRisas equipoComediantes =
    sum . map energiaDeRisa . concatMap (risas equipoComediantes)
    
------------
---Punto7---
------------

produccionEnergetica :: [a] -> (b -> Int) -> ([a] -> Ninio -> [b]) -> Campamento -> Int
produccionEnergetica equipo obtenerEnergia tipo =
    sum . map obtenerEnergia . concatMap (tipo equipo)
