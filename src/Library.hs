module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-------------- Parcial CraftMine --------------
{-
data Personaje = UnPersonaje {
    nombre :: String,
    puntaje :: Number,
    inventario :: [Material]
} deriving Show

type Material = String

lucho = UnPersonaje "Lucho" 1000 ["sueter", "fogata", "pollo", "pollo"]
fran = UnPersonaje "Fran" 1200 ["palo", "lana", "agujas", "tintura"]
data Crafteo = UnCrafteo {
    devuelve :: Material,
    recibe :: [Material],
    tiempo :: Number
} deriving Show

fogata = UnCrafteo "fogata" ["madera", "fosforo"] 10
polloAsado = UnCrafteo "pollo asado" ["fogata", "pollo"] 300
sueter = UnCrafteo "sueter" ["lana", "agujas", "tintura"] 600

-- Punto 1 --
nuevoObjeto :: Crafteo -> Personaje -> Personaje
nuevoObjeto crafteo personaje = personaje {inventario = (:) (devuelve crafteo) (inventario personaje)}

eliminarUnMaterial :: Eq a => a -> [a] -> [a]
eliminarUnMaterial _ [] = []
eliminarUnMaterial material (xMaterial:xMateriales)
    | material == xMaterial = xMateriales
    | otherwise = (xMaterial:eliminarUnMaterial material xMateriales)

eliminarMateriales :: Crafteo -> Personaje -> Personaje
eliminarMateriales crafteo personaje = personaje {inventario = foldr eliminarUnMaterial (inventario personaje) (recibe crafteo) }

puntosExtra :: Crafteo -> Personaje -> Personaje
puntosExtra crafteo personaje = personaje {puntaje = puntaje personaje + 10 * tiempo crafteo}

seEncuentra :: Material -> [Material] -> Bool
seEncuentra material materiales = elem material materiales

tieneMateriales :: Crafteo -> Personaje -> Bool
tieneMateriales crafteo personaje = all (flip seEncuentra (inventario personaje)) (recibe crafteo)

crafteo :: Crafteo -> Personaje -> Personaje
crafteo crafteo1 personaje
    | tieneMateriales crafteo1 personaje = puntosExtra crafteo1 (eliminarMateriales crafteo1 (nuevoObjeto crafteo1 personaje))
    | otherwise= personaje {puntaje = puntaje personaje - 100}
-- Punto 1 --

-- Punto 2 --
listaCrafteo1 :: [Crafteo]
listaCrafteo1 = [fogata, polloAsado]

crafteoDaDoblePuntaje :: Crafteo -> Personaje -> Bool
crafteoDaDoblePuntaje crafteo1 personaje = puntaje (crafteo crafteo1 personaje) > 2 * puntaje personaje

crafteosDoblePuntaje :: [Crafteo] -> Personaje -> [Crafteo]
crafteosDoblePuntaje crafteos personaje = filter (flip crafteoDaDoblePuntaje personaje) crafteos 

crafteoSucesivo :: [Crafteo] -> Personaje -> Personaje
crafteoSucesivo crafteos personaje = foldr crafteo personaje crafteos

quedaConMasPuntos :: [Crafteo] -> Personaje -> String
quedaConMasPuntos crafteos personaje
    | puntaje (crafteoSucesivo (reverse crafteos) personaje) > puntaje (crafteoSucesivo crafteos personaje) = "Orden inverso"
    | otherwise = "Orden indicado"  -- Siempre da en el orden indicado
-- Punto 2 --

-- Mine --
data Bioma = UnBioma {
    elementoNecesario :: String,
    materiales :: [Material]
}

artico = UnBioma "sueter" ["hielo", "iglues", "lobos"]

data Herramienta = UnaHerramienta {
    funcionHerramienta :: [Material] -> Material
}
hacha = UnaHerramienta funcionHacha
funcionHacha :: [Material] -> Material
funcionHacha = last

espada = UnaHerramienta funcionEspada
funcionEspada :: [Material] -> Material
funcionEspada = head

pico0 = UnaHerramienta funcionPico0
funcionPico0 :: [Material] -> Material
funcionPico0 materiales = (!!) materiales 0

pico1 = UnaHerramienta funcionPico1
funcionPico1 :: [Material] -> Material
funcionPico1 materiales = (!!) materiales 1

pico2 = UnaHerramienta funcionPico2
funcionPico2 :: [Material] -> Material
funcionPico2 materiales = (!!) materiales 2

-- Punto 2 --
pala = UnaHerramienta funcionPala
funcionPala :: [Material] -> Material
funcionPala materiales = (!!) materiales (length materiales / 2)

hazada = UnaHerramienta funcionHazada
funcionHazada :: [Material] -> Material
funcionHazada = (\ (m:mx:ms) -> mx)
-- Punto 2 --

-- Punto 1 --
minar :: Herramienta -> Personaje -> Bioma -> Personaje
minar herramienta personaje bioma
    | seEncuentra (elementoNecesario bioma) (inventario personaje) = personaje {puntaje = 50 + puntaje personaje, inventario = (:) (funcionHerramienta herramienta (materiales bioma)) (inventario personaje)}
    | otherwise = personaje
-- Punto 1 --

-- Punto 3 --
desierto = UnBioma "palo" (repeat "arena")

-- Analizando con el personaje Fran y la herramienta hacha explota el programa
-- Analizando con el personaje Fran y la herramienta espada le agrega arena al inventario de Fran
-- Analizando con el personaje Fran y cualquier pico como herramienta le agrega arena al inventario de Fran
-- Analizando con el personaje Fran y la herramienta hazada le agrega arena al inventario de Fran
-- Analizando con el personaje Fran y la herramienta pala explota el programa
-- Punto 3 --

-}
-------------- Parcial CraftMine --------------

-------------- Parcial Star Wars: Haskell Espacial --------------
data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poder :: Nave -> Nave
} deriving Show

ataqueCambio :: Number -> Nave -> Nave
ataqueCambio ataq nave
    | ataq > negate (ataque nave) = nave {ataque = ataque nave + ataq}
    | otherwise = nave {ataque = 0}

movTurbo :: Nave -> Nave
movTurbo = ataqueCambio 25 

repEmergencia :: Nave -> Nave
repEmergencia nave = ataqueCambio (-30) (durabilidadCambio 50 nave)

durabilidadCambio :: Number -> Nave -> Nave
durabilidadCambio dur nave
    | dur > negate (durabilidad nave) = nave {durabilidad = durabilidad nave + dur}
    | otherwise = nave {durabilidad = 0}

escudoCambio :: Number -> Nave -> Nave
escudoCambio esc nave
    | esc > negate (escudo nave) = nave {escudo = escudo nave + esc}
    | otherwise = nave {escudo = 0}

cazaTIE = UnaNave "Tie Fighter" 200 100 50 cazaTIEPoder
cazaTIEPoder :: Nave -> Nave
cazaTIEPoder = movTurbo

xWing = UnaNave "X Wing" 300 150 100 xWingPoder
xWingPoder :: Nave -> Nave
xWingPoder = repEmergencia

darthVader = UnaNave "Nave de Darth Vader" 500 300 200 darthVaderPoder
darthVaderPoder :: Nave -> Nave
darthVaderPoder darthVader = durabilidadCambio (-45) (movTurbo (movTurbo (movTurbo darthVader)))

halconMilenario = UnaNave "Millennium Falcon" 1000 500 50 halconPoder
halconPoder :: Nave -> Nave
halconPoder halcon = escudoCambio 100 (repEmergencia halcon)

-- Punto 1 --
bobaFett = UnaNave "Boba Fett" 600 350 275 bobaPoder
bobaPoder :: Nave -> Nave
bobaPoder boba = escudoCambio (-300) (ataqueCambio 225 boba)
-- Punto 1 --

-- Punto 2 --
flotaImperial :: [Nave]
flotaImperial = [cazaTIE, cazaTIE, cazaTIE, darthVader]
flotaRebelde :: [Nave]
flotaRebelde = [xWing, xWing, xWing, halconMilenario]

durabilidadTotal :: [Nave] -> Number
durabilidadTotal flota = sum (map durabilidad flota)
-- Punto 2 --

-- Punto 3 --
ataqueNave :: Nave -> Nave -> Nave
ataqueNave atacante defensor
    | ataque (poder atacante atacante) < escudo (poder defensor defensor) = ataqueEscudo atacante defensor
    | otherwise = durabilidadCambio (diferenciaEscudo atacante defensor) (ataqueEscudo atacante defensor)

diferenciaEscudo :: Nave -> Nave -> Number
diferenciaEscudo atacante defensor = negate (ataque(poder atacante atacante) - escudo (poder defensor defensor))

ataqueEscudo :: Nave -> Nave -> Nave
ataqueEscudo atacante defensor = escudoCambio (negate (ataque (poder atacante atacante))) (poder defensor defensor)
-- Punto 3 --

-- Punto 4 --
fueraDeCombate :: Nave -> Nave -> Bool
fueraDeCombate atacante defensor = durabilidad (ataqueNave atacante defensor) == 0
-- Punto 4 --

-- Punto 5 --
type Estrategia = Nave -> Bool

navesDebiles :: Estrategia 
navesDebiles defensor = 200 > escudo defensor

navesConCiertaPeligrosidad :: Number -> Estrategia
navesConCiertaPeligrosidad peligrosidad defensor = peligrosidad < ataque defensor

navesFueraDeCombate :: Nave -> Estrategia
navesFueraDeCombate = fueraDeCombate 

navesDebiles' :: Estrategia
navesDebiles' defensor = 300 >= durabilidad defensor

misionSorpresa :: Estrategia -> Nave -> [Nave] -> [Nave]
misionSorpresa estrategia atacante flota = map (ataqueNave atacante) (filter estrategia flota)
-- Punto 5 --

-- Punto 6 --
mejorEstrategia :: Estrategia -> Estrategia -> Nave -> [Nave] -> String
mejorEstrategia estrategia1 estrategia2 atacante flota 
    | durabilidadTotal (misionSorpresa estrategia1 atacante flota) > durabilidadTotal (misionSorpresa estrategia2 atacante flota) = "La Estrategia N°1 es mejor"
    | otherwise = "La Estrategia N°2 es mejor" 
-- Punto 6 --

-- Punto 7 --
flotaInfinita :: [Nave]
flotaInfinita = repeat cazaTIE
{-No es posible determinar su durabilidad total debido a que se necesita de toda la lista para poder calcular la suma de las durabilidades de cada nave individual pero la lista al no tener fin es imposible
Cuando se intenta llevar a cabo una misión sorpresa lo que ocurre es que se empiezan a hacer chequeos infinitos según que estrategia y por así decirlo explota el código iterando infinitamente-}
-- Punto 7 --

-------------- Parcial Star Wars: Haskell Espacial --------------