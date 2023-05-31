{- Punto 1 -}

data Personaje = Personaje
  { nombre :: String,
    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    planeta :: String
  }

data Guantelete = Guantelete
  { material :: String,
    gemas :: [Gema]
  }

type Gema = Habilidad

type Universo = [Personaje]

type Habilidad = Personaje -> Personaje

chasquidoUniverso :: Guantelete -> Universo -> Universo
chasquidoUniverso guantelete universoOriginal
  | puedeChasquear guantelete = reducirPersonajesAMitad universoOriginal
  | otherwise = universoOriginal

puedeChasquear :: Guantelete -> Bool
puedeChasquear guantelete = ((length . gemas $ guantelete) == 6) && (material guantelete == "uru")

reducirPersonajesAMitad :: Universo -> Universo
reducirPersonajesAMitad universoOriginal = take (div (length universoOriginal) 2) universoOriginal

{- Punto 2 -}
esUniversoAptoParaPendex :: Universo -> Bool
esUniversoAptoParaPendex universo = any esPendex universo

esPendex :: Personaje -> Bool
esPendex = (< 45) . edad

energiaTotalUniverso :: Universo -> Int
energiaTotalUniverso universo = sum (map energia (filter tieneMasDeUnaHabilidad universo))

tieneMasDeUnaHabilidad :: Personaje -> Bool
tieneMasDeUnaHabilidad = (> 1) . length . habilidades

{- Punto 3 -}
mente :: Int -> Gema
mente n unPersonaje = bajarEnergia n unPersonaje

bajarEnergia :: Int -> Personaje -> Personaje
bajarEnergia n unPersonaje = unPersonaje {energia = energia unPersonaje - n}

alma :: String -> Gema
alma habilidad unPersonaje = bajarEnergia 10 unPersonaje {habilidades = eliminarHabilidad habilidad (habilidades unPersonaje)}

-- alma habilidad unPersonaje = unPersonaje {habilidades = eliminarHabilidad habilidad (habilidades unPersonaje), energia = energia unPersonaje - 10}

eliminarHabilidad :: String -> [String] -> [String]
eliminarHabilidad habilidad habilidades = filter (/= habilidad) habilidades

espacio :: String -> Gema
espacio nuevoPlaneta unPersonaje = bajarEnergia 20 unPersonaje {planeta = nuevoPlaneta}

poder :: Gema
poder unPersonaje
  | tieneMax2Habilidades unPersonaje = eliminarHabilidades . bajarEnergia (energia unPersonaje) $ unPersonaje
  | otherwise = bajarEnergia (energia unPersonaje) unPersonaje

tieneMax2Habilidades :: Personaje -> Bool
tieneMax2Habilidades = (<= 2) . length . habilidades

eliminarHabilidades :: Personaje -> Personaje
eliminarHabilidades unPersonaje = unPersonaje {habilidades = []}

tiempo :: Gema
tiempo unPersonaje = bajarEnergia 50 unPersonaje {edad = reducirEdadMitad (edad unPersonaje)}

reducirEdadMitad :: Int -> Int
reducirEdadMitad edad = max (div edad 2) 18

gemaLoca :: Gema -> Gema
gemaLoca gema = gema . gema

{- Punto 4 -}
guanteleteDeGoma :: Guantelete
guanteleteDeGoma = Guantelete "Goma" [tiempo, alma "usar Mjolnir", gemaLoca (alma "programación en Haskell")]

{- Punto 5 -}
utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas enemigo = foldl (\e gema -> gema e) enemigo gemas

{- Punto 6 -}
gemaMasPoderosa :: Personaje -> Guantelete -> Gema
gemaMasPoderosa personaje guantelete = gemaMasPoderosaDe personaje (gemas guantelete)

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe personaje (gema1 : gema2 : gemas)
  | (energia . gema1) personaje < (energia . gema2) personaje = gemaMasPoderosaDe personaje (gema1 : gemas)
  | otherwise = gemaMasPoderosaDe personaje (gema2 : gemas)

{- Punto 7 -}
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema : (infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3 . gemas) guantelete

-- 1) gemaMasPoderosa punisher guanteleteDeLocos no se

-- 2) usoLasTresPrimerasGemas guanteleteDeLocos punisher -- Si se puede ejecutar ya que, aunque el guantelete tenga gemas infinitas,
-- si se pueden agarrar las 3 primeras gemas y aplicarcelas a punisher