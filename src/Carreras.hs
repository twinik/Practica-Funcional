------------
---Punto1---
------------

data Auto = Auto {
    color :: String,
    velocidad :: Int,
    distancia :: Int
}

type Carrera = [Auto]

--A
estanCerca :: Auto -> Auto -> Bool
estanCerca auto1 auto2 = 
    sonDistintos auto1 auto2 && distanciaMenorA10 auto1 auto2

sonDistintos :: Auto -> Auto
sonDistintos auto1 auto2 = color auto1 /= color auto2

distanciaMenorA10 :: Auto -> Auto
distanciaMenorA10 auto1 auto2 = abs (distancia auto1 - distancia auto2) < 10

--B
vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo unAuto carrera = 
    noTieneNingunoCerca unAuto carrera && vaGanando unAuto carrera

noTieneNingunoCerca :: Auto -> Carrera -> Bool
noTieneNingunoCerca unAuto carrera = not (any (estanCerca unAuto) carrera)

vaGanando :: Auto -> Carrera -> Bool
vaGanando unAuto carrera = distancia unAuto > maximum (map distancia carrera)
-- all (\autoEnCarrera -> distancia unAuto > distancia autoEnCarrera) (map distancia carrera)

--C
enQuePuestoEsta :: Auto -> Carrera -> Int
enQuePuestoEsta unAuto = (+1) . length . filter (vaMejor unAuto)

vaMejor :: Auto -> Auto -> Bool
vaMejor autoPeor autoMejor = distancia autoMejor > distancia autoPeor

------------
---Punto2---
------------