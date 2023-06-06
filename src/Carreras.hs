------------
---Punto1---
------------

data Auto = Auto {
    color :: Color,
    velocidad :: Int,
    distancia :: Int
} deriving (Show, Eq)

data Color = Rojo | Blanco | Azul | Negro deriving (Show, Eq)

type Carrera = [Auto]

--A
estanCerca :: Auto -> Auto -> Bool
estanCerca auto1 auto2 = 
    sonDistintos auto1 auto2 && distanciaMenorA10 auto1 auto2

sonDistintos :: Auto -> Auto -> Bool
sonDistintos auto1 auto2 = color auto1 /= color auto2

distanciaMenorA10 :: Auto -> Auto -> Bool
distanciaMenorA10 auto1 auto2 = abs (distancia auto1 - distancia auto2) < 10

--B
vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo unAuto carrera = 
    noTieneNingunoCerca unAuto carrera && vaGanando unAuto carrera

noTieneNingunoCerca :: Auto -> Carrera -> Bool
noTieneNingunoCerca unAuto carrera = not (any (estanCerca unAuto) carrera)

vaGanando :: Auto -> Carrera -> Bool
vaGanando unAuto = all (leGana unAuto)

leGana :: Auto -> Auto -> Bool
leGana auto1 auto2 = distancia auto1 > distancia auto2

--C
enQuePuestoEsta :: Auto -> Carrera -> Int
enQuePuestoEsta unAuto = (+1) . length . filter (vaMejor unAuto)

vaMejor :: Auto -> Auto -> Bool
vaMejor autoPeor autoMejor = distancia autoMejor > distancia autoPeor

------------
---Punto2---
------------
--A
correrPorDeterminadoTiempo :: Int -> Auto -> Auto
correrPorDeterminadoTiempo tiempo unAuto = 
    modificarDistancia (+ (tiempo * velocidad unAuto)) unAuto

modificarDistancia :: (Int -> Int) -> Auto -> Auto
modificarDistancia funcion unAuto = 
    unAuto {distancia = funcion . distancia $ unAuto}

--B
modificarVelocidad :: (Int -> Int) -> Auto -> Auto
modificarVelocidad funcion unAuto = 
    unAuto {velocidad = max (funcion . velocidad $ unAuto) 0}

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cantidad = modificarVelocidad (+ (-cantidad))

------------
---Punto3---
------------

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

type PowerUp =  Carrera -> Auto -> Carrera

--A
terremoto :: PowerUp
terremoto carrera unAuto = 
    afectarALosQueCumplen (estanCerca unAuto) (bajarVelocidad 50) carrera

--B
miguelitos :: Int -> PowerUp
miguelitos cantidad carrera unAuto = 
    afectarALosQueCumplen (leGana unAuto) (bajarVelocidad cantidad) carrera

--C
jetPack :: Int -> PowerUp
jetPack duracion carrera unAuto = 
    afectarALosQueCumplen (== unAuto) (powerUpJetPack duracion) carrera
    
powerUpJetPack :: Int -> Auto -> Auto
powerUpJetPack duracion = 
    modificarVelocidad (flip div 2) . correrPorDeterminadoTiempo duracion . modificarVelocidad (*2)

------------
---Punto4---
------------
--A
simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
simularCarrera carrera eventos = (obtenerPosiciones . producirEventos eventos) carrera

type Evento = Carrera -> Carrera

producirEventos :: [Evento] -> Carrera -> Carrera
producirEventos eventos carrera = foldr ($) carrera eventos

obtenerPosiciones :: Carrera -> [(Int, Color)]
obtenerPosiciones carrera = 
    map (\auto -> (enQuePuestoEsta auto carrera, color auto)) carrera

--B
correnTodos :: Int -> Evento
correnTodos tiempo = map (correrPorDeterminadoTiempo tiempo)

usaPowerUp :: PowerUp -> Color -> Evento
usaPowerUp powerUp color carrera = powerUp carrera (encontrarAuto color carrera) 

encontrarAuto :: Color -> Carrera -> Auto
encontrarAuto unColor = head . filter (\auto -> color auto == unColor)

--C

laCarrera :: Carrera
laCarrera = [
    Auto Rojo 120 0,
    Auto Blanco 120 0,
    Auto Azul 120 0,
    Auto Negro 120 0 ]

losEventos :: [Evento]
losEventos = [
    correnTodos 30,
    usaPowerUp (jetPack 3) Azul,
    usaPowerUp terremoto Blanco,
    correnTodos 40,
    usaPowerUp (miguelitos 20) Blanco,
    usaPowerUp (jetPack 6) Negro,
    correnTodos 10 ]

{- 
    ghci> simularCarrera laCarrera losEventos
    [(3,Azul),(4,Rojo),(1,Negro),(2,Blanco)] 
-}

------------
---Punto5---
------------

{-
--A
Si lo permite

--B
No se terminarian de evaluar nunca ya que necesita 
la informacion de todos los autos de la carrera
-}