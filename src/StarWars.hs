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
saberComoQuedaNaveAtacada naveAtacante naveAtacada
    | danioRecibido naveAtacada naveAtacante <= durabilidad naveAtacada = modificarDurabilidad ((-) (danioRecibido naveAtacada naveAtacante)) naveAtacada
    | otherwise = modificarDurabilidad ((-) durabilidad naveAtacada) naveAtacada

danioRecibido :: Nave -> Nave -> Int
danioRecibido naveAtacante naveAtacada
    | calculoDanio naveAtacante naveAtacada > 0 = calculoDanio naveAtacante naveAtacada
    | otherwise = 0

calculoDanio :: Nave -> Nave -> Int
calculoDanio naveAtacante naveAtacada = ataque . activarPoder $ naveAtacante - escudo . activarPoder $ naveAtacada

activarPoder :: Nave -> Nave
activarPoder = poderEspecial unaNave

------------
---Punto4---
------------

naveFueraDeCombate :: Nave -> Bool
naveFueraDeCombate = (== 0) . durabilidad