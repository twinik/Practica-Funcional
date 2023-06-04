import Text.Show.Functions

------------
---Punto1---
------------

data Turista = Turista {
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving (Show, Eq)

ana = Turista 0 21 False ["Espaniol"]

beto = Turista 15 15 True ["Aleman"]

cathi = Turista 15 15 True ["Aleman", "Catalan"]

------------
---Punto2---
------------

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya unTurista
    | viajaSolo unTurista = cambiarCansancio (+(-5)) unTurista
    | otherwise = cambiarStress (+(-1)) unTurista

apreciarElementoPaisaje :: String -> Excursion
apreciarElementoPaisaje elemento = cambiarStress (+(-(length elemento)))

salirAHablarUnIdioma :: String -> Excursion
salirAHablarUnIdioma unIdioma = 
    cambiarSiEstaAcompaniado . agregarIdioma unIdioma

caminar :: Int -> Excursion
caminar minutos = 
    cambiarStress (+(-(intensidad minutos))) . cambiarCansancio (+ (intensidad minutos)) 

intensidad :: Int -> Int
intensidad minutos = div minutos 4

data Marea = Fuerte | Moderada | Tranquila deriving (Eq)

paseoEnBarco :: Marea -> Excursion
paseoEnBarco marea unTurista
    | marea == Fuerte = cambiarStress (+6) . cambiarCansancio (+10) $ unTurista
    | marea == Tranquila = (caminar 10 . apreciarElementoPaisaje "mar" . salirAHablarUnIdioma "Aleman") unTurista
    | otherwise = unTurista

cambiarCansancio :: (Int -> Int) -> Turista -> Turista
cambiarCansancio funcion unTurista = unTurista {cansancio = funcion . cansancio $ unTurista}

cambiarStress :: (Int -> Int) -> Turista -> Turista
cambiarStress funcion unTurista = unTurista {stress = funcion . stress $ unTurista}

cambiarSiEstaAcompaniado :: Turista -> Turista
cambiarSiEstaAcompaniado unTurista = unTurista {viajaSolo = False}

agregarIdioma :: String -> Turista -> Turista
agregarIdioma unIdioma unTurista 
    | elem unIdioma (idiomas unTurista) = unTurista
    | otherwise = unTurista {idiomas = unIdioma : idiomas unTurista}

--A

hacerExcursion :: Turista -> Excursion -> Turista
hacerExcursion unTurista unaExcursion = cambiarStress (+(-(stress unTurista * 10 `div` 100))) . unaExcursion $ unTurista

--B

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

type Indice = Turista -> Int

deltaExcursionSegun :: Indice -> Turista -> Excursion -> Int
deltaExcursionSegun unIndice unTurista unaExcursion = 
    deltaSegun unIndice (hacerExcursion unTurista unaExcursion) unTurista 

--C.i

esExcursionEducativa :: Excursion -> Turista -> Bool
esExcursionEducativa unaExcursion unTurista = 
    varioDelta (deltaExcursionSegun (length . idiomas) unTurista unaExcursion)

varioDelta :: Int -> Bool
varioDelta = (>0)


--C.ii

excrusionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excrusionesDesestresantes unTurista todasLasExcursiones = 
    filter (esDesestresante unTurista) todasLasExcursiones

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante unTurista unaExcursion = 
    (deltaExcursionSegun stress unTurista unaExcursion) <= (-3)

------------
---Punto3---
------------


