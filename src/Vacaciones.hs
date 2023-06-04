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

esExcursionEducativa :: Turista -> Excursion -> Bool
esExcursionEducativa unTurista = (>0) . deltaExcursionSegun (length . idiomas) unTurista

--C.ii

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes unTurista = filter (esDesestresante unTurista) 

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante unTurista unaExcursion = 
    (deltaExcursionSegun stress unTurista unaExcursion) <= (-3)

------------
---Punto3---
------------

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarElementoPaisaje "cascada", caminar 40, salirAHablarUnIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursionElegida = [paseoEnBarco Tranquila, excursionElegida, caminar 120]

islaVecina :: Marea -> Tour
islaVecina marea
    | marea == Fuerte = [paseoEnBarco marea, apreciarElementoPaisaje "lago", paseoEnBarco marea]
    | otherwise = [paseoEnBarco marea, irALaPlaya, paseoEnBarco marea]

--A

hacerTour :: Tour -> Turista -> Turista
hacerTour unTour = cambiarStress (+ (length unTour)) . completarExcursiones unTour

completarExcursiones :: Tour -> Turista -> Turista
completarExcursiones unTour unTurista = foldr ($) unTurista unTour

--B 

existeTourConvincente :: Turista -> [Tour] -> Bool
existeTourConvincente unTurista listaTours = 
    existeTourDesestresante unTurista listaTours && viajaAcompaniadoDespuesDeTourDesestresante (tourDesestresante unTurista listaTours) unTurista

existeTourDesestresante :: Turista -> [Tour] -> Bool 
existeTourDesestresante unTurista listaTours = any (esTourDesestresante unTurista) listaTours

esTourDesestresante :: Turista -> Tour -> Bool
esTourDesestresante unTurista unTour = length (excursionesDesestresantes unTurista unTour) > 0

viajaAcompaniadoDespuesDeTourDesestresante :: Tour -> Turista -> Bool
viajaAcompaniadoDespuesDeTourDesestresante unTour =
    not . viajaSolo . hacerTour unTour

tourDesestresante :: Turista -> [Tour] -> Tour
tourDesestresante unTurista listaTours = head . filter (esTourDesestresante unTurista) $ listaTours

--C

efectividadDelTour :: Tour -> [Turista] -> Int
efectividadDelTour unTour = sum . listaEspiritualidades unTour

listaEspiritualidades :: Tour -> [Turista] -> [Int]
listaEspiritualidades unTour =
    map espiritualidadRecibida unTour . filter (existeTourConvincente [unTour])

espiritualidadRecibida :: Tour -> Turista -> Int
espiritualidadRecibida unTour unTurista = 
    deltaTourSegun stress (hacerTour unTour unTurista) unTour + deltaTourSegun cansancio (hacerTour unTour unTurista) unTour

deltaTourSegun :: Indice -> Turista -> Tour -> Int
deltaTourSegun unIndice unTurista unTour = 
    deltaSegun unIndice (hacerTour unTurista unTour) unTurista 

------------
---Punto4---
------------

--A
tourInfinito :: Tour
tourInfinito = irALaPlaya : repeat irALaPlaya

--B
--Para Ana sí porque la primer actividad ya es desestresante y siempre está acompañada.
--Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.

--C
{- No existe, ya que para saber la efectividad de un tour hay que completar
el tour, y en este caso al ser infinito, nunca se completaria -}