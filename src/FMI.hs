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
namibia :: Pais 4140 400000 650000 ["Mineria", "Ecoturismo"] 50

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

cambiarIngresoPerCapita :: (Int -> Int) -> Pais -> Pais
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
prestarMitadPBI unPais = prestarMillones (mitadPBI unPais) unPais

mitadPBI :: Pais -> Int
mitadPBI unPais = 
    div (ingresoPerCapita unPais * (sectorPrivado unPais + sectorPublico unPais)) 2

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




