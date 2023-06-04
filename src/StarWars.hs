------------
---Punto1---
------------

data Nave = Nave {
    nombre :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poderEspecial :: Poder
}

type Poder = Nave -> Nave

tieFighter :: Nave
tieFighter = Nave "TIE Fighter" 200 100 50 turbo

xWing :: Nave
xWing = Nave "X Wing" 300 150 100 reparacionEmergencia

naveDarthVader :: Nave
naveDarthVader = Nave "Nave de Darth Vader" 500 300 200 superTurbo

millenniumFalcon :: Nave
millenniumFalcon = Nave "Millennium Falcon" 1000 500 50 (modificarEscudo (+100) . reparacionEmergencia)

naveIncreible :: Nave
naveIncreible = Nave "Nave Increible" 3000 1500 2500 velocidadLuz

turbo :: Poder
turbo = modificarAtaque (+25) 

reparacionEmergencia :: Poder
reparacionEmergencia = modificarDurabilidad (+50) . modificarAtaque ((-)30)

superTurbo :: Poder
superTurbo = turbo . turbo . turbo . modificarDurabilidad ((-)45)

velocidadLuz :: Poder
velocidadLuz = modificarAtaque (+2500) . modificarDurabilidad (*2) . modificarEscudo (+1500)

modificarAtaque :: (Int -> Int) -> Nave -> Nave
modificarAtaque funcion unaNave = unaNave {ataque = funcion . ataque $ unaNave}

modificarDurabilidad :: (Int -> Int) -> Nave -> Nave
modificarDurabilidad funcion unaNave = unaNave {durabilidad = funcion . durabilidad $ unaNave}

modificarEscudo :: (Int -> Int) -> Nave -> Nave
modificarEscudo funcion unaNave = unaNave {escudo = funcion . escudo $ unaNave}

--modificarEscudo funcion unaNave = modificar escudo funcion unaNave

--modificar :: a -> (Int -> Int) -> Nave -> Nave
--modificar dato funcion unaNave = unaNave {dato = funcion . dato $ unaNave}

------------
---Punto2---
------------

type Flota = [Nave]

durabilidadTotal :: Flota -> Int
durabilidadTotal = sum . map durabilidad

------------
---Punto3---
------------

saberComoQuedaNaveAtacada :: Nave -> Nave -> Nave
saberComoQuedaNaveAtacada naveAtacante naveAtacada = modificarDurabilidad ((-) (min (durabilidad naveAtacada) (danioRecibido naveAtacada naveAtacante)))  (activarPoder naveAtacada)

danioRecibido :: Nave -> Nave -> Int
danioRecibido naveAtacante naveAtacada = max (calculoDanio naveAtacante naveAtacada) 0

calculoDanio :: Nave -> Nave -> Int
calculoDanio naveAtacante naveAtacada = ataque (activarPoder naveAtacante) - escudo (activarPoder naveAtacada)

activarPoder :: Nave -> Nave
activarPoder unaNave = (poderEspecial unaNave) unaNave

------------
---Punto4---
------------

naveFueraDeCombate :: Nave -> Bool
naveFueraDeCombate = (== 0) . durabilidad

------------
---Punto5---
------------

type Estrategia = Nave -> Bool

misionSorpresa :: Flota -> Nave -> Estrategia -> Flota
misionSorpresa flotaEnemiga naveAtacante estrategia = map (atacarSiCumplenEstrategia estrategia naveAtacante) flotaEnemiga

atacarSiCumplenEstrategia :: Estrategia -> Nave -> Nave -> Nave
atacarSiCumplenEstrategia estrategia naveAtacante naveAtacada
    | estrategia naveAtacada = saberComoQuedaNaveAtacada naveAtacante naveAtacada
    | otherwise = naveAtacada

navesDebiles :: Estrategia
navesDebiles = (<200) . escudo

navesConCiertaPeligrosidad :: Int -> Estrategia
navesConCiertaPeligrosidad valorDado = (>valorDado) . ataque 

navesQueQuedarianFueraDeCombate :: Nave -> Estrategia
navesQueQuedarianFueraDeCombate naveAtacante = naveFueraDeCombate . (saberComoQuedaNaveAtacada naveAtacante)

navesMuyPeligrosas :: Estrategia
navesMuyPeligrosas unaNave = (ataque unaNave) > 500 && (escudo unaNave) > 300

------------
---Punto6---
------------

usarMejorEstrategia :: Nave -> Flota -> Estrategia -> Estrategia -> Flota
usarMejorEstrategia naveAtacante flotaEnemiga estrategia1 estrategia2 = 
    misionSorpresa flotaEnemiga naveAtacante (mejorEstrategia naveAtacante flotaEnemiga estrategia1 estrategia2)

mejorEstrategia :: Nave -> Flota -> Estrategia -> Estrategia -> Estrategia
mejorEstrategia naveAtacante flotaEnemiga estrategia1 estrategia2
    | esMejor naveAtacante flotaEnemiga estrategia1 estrategia2 = estrategia1
    | otherwise = estrategia2

esMejor :: Nave -> Flota -> Estrategia -> Estrategia -> Bool
esMejor naveAtacante flotaEnemiga mejorEstrategia peorEstrategia =
    (durabilidadTotal (misionSorpresa flotaEnemiga naveAtacante mejorEstrategia)) < (durabilidadTotal (misionSorpresa flotaEnemiga naveAtacante peorEstrategia))

------------
---Punto7---
------------
-- No es posible determinar su durabilidad total ya que se tendria que sumar la durabilidad de infinitas naves, lo cual no terminaria nunca
-- Cuando se lleva una mision sobre ella, atacaria a todas las naves que cumplan la estrategia pero al ser naves infinitas, nunca devolveria
-- la flota completamente atacada ya que nunca terminaria
