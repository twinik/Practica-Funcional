{- Héroes de Leyenda -}

{- Punto 1 -}

data Heroe = Heroe
  { epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
  }

data Artefacto = Artefacto
  { nombre :: String,
    rareza :: Int
  }

type Tarea = Heroe -> Heroe

{- Punto 2 -}

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria unHeroe
  | reconocimiento unHeroe > 1000 = cambiarEpiteto "El mítico" unHeroe
  | reconocimiento unHeroe >= 500 = cambiarEpiteto "El magnífico" . agregarArtefacto lanzaDelOlimpo $ unHeroe
  | reconocimiento unHeroe > 100 = cambiarEpiteto "Hoplita" . agregarArtefacto xiphos $ unHeroe
  | otherwise = unHeroe

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "Lanza del Olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50

cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto unEpiteto unHeroe = unHeroe {epiteto = unEpiteto}

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto unArtefacto unHeroe = unHeroe {artefactos = unArtefacto : artefactos unHeroe}

{- Punto 3 -}

-- Tarea 1
encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto unArtefacto unHeroe = ganarReconocimiento (rareza unArtefacto) . agregarArtefacto unArtefacto $ unHeroe

ganarReconocimiento :: Int -> Heroe -> Heroe
ganarReconocimiento unReconocimiento unHeroe = unHeroe {reconocimiento = reconocimiento unHeroe + unReconocimiento}

-- Tarea 2
escalarElOlimpo :: Tarea
escalarElOlimpo unHeroe = agregarArtefacto elRelampagoDeZeus . desecharArtefactos . triplicarRarezaArtefactos . ganarReconocimiento 500 $ unHeroe

elRelampagoDeZeus :: Artefacto
elRelampagoDeZeus = Artefacto "El relámpago de Zeus" 500

triplicarRarezaArtefactos :: Heroe -> Heroe
triplicarRarezaArtefactos unHeroe = unHeroe {artefactos = map (\artefacto -> artefacto {rareza = rareza artefacto * 3}) (artefactos unHeroe)}

desecharArtefactos :: Heroe -> Heroe
desecharArtefactos unHeroe = unHeroe {artefactos = filter ((>= 1000) . rareza) (artefactos unHeroe)}

-- Tarea 3
ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cuadras unHeroe = cambiarEpiteto ("Gros" ++ replicate cuadras 'o') unHeroe

-- Tarea 4
matarUnaBestia :: Bestia -> Tarea
matarUnaBestia unaBestia unHeroe
  | debilidad unaBestia unHeroe = cambiarEpiteto ("El asesino de " ++ nombreBestia unaBestia) unHeroe
  | otherwise = cambiarEpiteto "El cobarde" . perderPrimerArtefacto $ unHeroe

data Bestia = Bestia
  { nombreBestia :: String,
    debilidad :: Debilidad
  }

type Debilidad = Heroe -> Bool

perderPrimerArtefacto :: Heroe -> Heroe
perderPrimerArtefacto unHeroe = unHeroe {artefactos = drop 1 (artefactos unHeroe)}

{- Punto 4 -}
heracles :: Heroe
heracles = Heroe "Guardián del Olimpo" 700 [fierro, elRelampagoDeZeus] [matarAlLeonDeNemea]

fierro :: Artefacto
fierro = Artefacto "El Fierro" 1000

{- Punto 5 -}
matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea = matarUnaBestia leonDeNemea

leonDeNemea :: Bestia
leonDeNemea = Bestia "Leon de Nemea" ((>= 20) . length . epiteto)

{- Punto 6 -}
hacerUnaTarea :: Tarea -> Heroe -> Heroe
hacerUnaTarea unaTarea unHeroe = (unaTarea unHeroe) {tareas = unaTarea : tareas unHeroe}

{- Punto 7 -}
presumir :: Heroe -> Heroe -> (Heroe, Heroe)
presumir h1 h2
  | leGana h1 h2 = (h1, h2)
  | leGana h2 h1 = (h2, h1)
  | otherwise = presumir (hacerTareas (tareas h1) h2) (hacerTareas (tareas h2) h1)

leGana :: Heroe -> Heroe -> Bool
leGana h1 h2 =
  reconocimiento h1 > reconocimiento h2
    || (reconocimiento h1 == reconocimiento h2 && sumatoriaRarezasArtefactos h1 > sumatoriaRarezasArtefactos h2)

sumatoriaRarezasArtefactos :: Heroe -> Int
sumatoriaRarezasArtefactos unHeroe = sum . map rareza $ (artefactos unHeroe)

hacerTareas :: [Tarea] -> Heroe -> Heroe
hacerTareas unasTareas unHeroe = foldl (flip hacerUnaTarea) unHeroe unasTareas

{- Punto 8 -}
-- Entra en un loop infinito

{- Punto 9 -}
type Labor = [Tarea]

realizarLabor :: Labor -> Heroe -> Heroe
realizarLabor unaLabor unHeroe = hacerTareas unaLabor unHeroe

{- Punto 10 -}
-- No se podra, ya que va a hacer un loop de tareas infinitas y nunca va a terminar

---------------------------------------------------------------------------------------------------