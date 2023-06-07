------------
---Punto1---
------------

--A
data Pais = Pais {
    ingresoPerCapita :: Float,
    sectorPublico:: Int,
    sectorPrivado :: Int,
    recursosNaturales :: [String],
    deuda :: Int
}

--B
namibia :: Pais 
namibia = Pais 4140 400000 650000 ["Mineria", "Ecoturismo"] 50

------------
---Punto2---
------------

type Estrategia = Pais -> Pais

prestarMillones :: Int -> Estrategia
prestarMillones millones = contraerDeuda millones

contraerDeuda :: Int -> Pais -> Pais
contraerDeuda millones unPais = 
    unPais {deuda = deuda unPais + (millones * 150 `div` 100) }

reducirSectorPublico :: Int -> Estrategia
reducirSectorPublico cantidad unPais
    | sectorPublico unPais > 100 = cambiarSectorPublico (-cantidad) . cambiarIngresoPerCapita (*0.8) $ unPais
    | otherwise = cambiarSectorPublico (-cantidad) . cambiarIngresoPerCapita (*0.85) $ unPais

cambiarSectorPublico :: Int -> Pais -> Pais
cambiarSectorPublico cantidad unPais =
    unPais {sectorPublico = sectorPublico unPais + cantidad }

cambiarIngresoPerCapita :: (Float -> Float) -> Pais -> Pais
cambiarIngresoPerCapita f unPais = 
    unPais {ingresoPerCapita = f (ingresoPerCapita unPais)}

darExplotacion :: String -> Estrategia
darExplotacion recursosNatural = 
    cambiarDeuda (-2) . dejarSinRecurso recursosNatural

cambiarDeuda :: Int -> Pais -> Pais
cambiarDeuda cantidad unPais =
    unPais {deuda = deuda unPais + cantidad }

dejarSinRecurso :: String -> Pais -> Pais
dejarSinRecurso recursosNatural unPais = 
    unPais {recursosNaturales = filter (/= recursosNatural) (recursosNaturales unPais)}

establecerBlindaje :: Estrategia
establecerBlindaje = 
    prestarMitadPBI . cambiarSectorPublico (-500)

prestarMitadPBI :: Pais -> Pais
prestarMitadPBI unPais = prestarMillones (div (pbi unPais) 2) unPais

pbi :: Pais -> Int
pbi unPais = 
    ingresoPerCapita unPais * (sectorPrivado unPais + sectorPublico unPais)

------------
---Punto3---
------------

type Receta = [Estrategia]

--A
receta1 :: Receta
receta1 = [prestarMillones 200, darExplotacion "Mineria"]

--B
aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldr ($) pais receta

aplicarRecetaANamibia :: Pais
aplicarRecetaANamibia = aplicarReceta receta1 namibia

------------
---Punto4---
------------
--A
puedenZafar :: [Pais] -> [Pais]
puedenZafar = 
    filter ((>0) . length . filter (== "Petroleo") . recursosNaturales)

--B
totalDeudaAFavor :: [Pais] -> Int
totalDeudaAFavor = sum . map deuda

--C
{-
En el punto A aparecio el concepto de composicion, ya que se compusieron varias 
funciones para lograr filtar los paises que tienen petroleo en sus recursos y
aparecio el concepto de aplicacion parcial, ya que se aplicaron las funciones
parcialmente para poder componerlas

En el punto B se aparecio el concepto de orden superior ya que se uso una funcion que
se le pasa otra funcion por parametro y el concepto de composicion, ya que se compuso
la lista de deudas obtenidas con el map, con el sum, el cual suma esa lista
-}

------------
---Punto5---
------------
estaOrdenado :: Pais -> [Receta] -> Bool
estaOrdenado pais [receta] = True
estaOrdenado pais (receta1:receta2:recetas)
     = revisarPBI receta1 pais <= revisarPBI receta2 pais && estaOrdenado pais (receta2:recetas)
     where revisarPBI receta = pbi . aplicarReceta receta


