import Text.Show.Functions

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

{- Punto 1 -}
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro {velocidad = 10, precision = precisionJugador habilidad * 2, altura = 0}

madera :: Palo
madera habilidad = UnTiro {velocidad = 100, precision = div (precisionJugador habilidad) 2, altura = 5}

hierros :: Int -> Palo
hierros n habilidad = UnTiro {velocidad = fuerzaJugador habilidad * n, precision = div (precisionJugador habilidad) n, altura = (n-3)}

palos :: [Palo]
palos = [putter, madera] ++ map hierros [1..10]

{- Punto 2 -}
golpe :: Jugador -> Palo -> Tiro
golpe unJugador unPalo = unPalo (habilidad unJugador) 

{- Punto 3 -}
data Obstaculo = Obstaculo {
    condicion :: (Tiro -> Bool),
    efecto :: (Tiro -> Tiro)
}

tiroDetenido = UnTiro 0 0 0

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo unObstaculo unTiro
    | condicion unObstaculo unTiro = efecto unObstaculo unTiro
    | otherwise = tiroDetenido

tunelConRampita :: Obstaculo
tunelConRampita = Obstaculo superaTunelConRampita efectoTunelConRampita

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && altura tiro == 0

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = UnTiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}

laguna :: Int -> Obstaculo 
laguna largo = Obstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && between 1 5 (altura tiro)

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiro = tiro {altura = div (altura tiro) largo}

hoyo :: Obstaculo
hoyo = Obstaculo superaHoyo efectoHoyo

superaHoyo :: Tiro -> Bool
superaHoyo tiro = between 5 20 (velocidad tiro) && altura tiro == 0 && precision tiro > 95

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroDetenido

{- Punto 4 -}
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (esPaloUtil unJugador unObstaculo) palos

esPaloUtil :: Jugador -> Obstaculo -> Palo -> Bool
esPaloUtil unJugador unObstaculo unPalo = condicion unObstaculo (golpe unJugador unPalo)

