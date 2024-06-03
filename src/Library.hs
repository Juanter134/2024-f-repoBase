module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-------------- Parcial CraftMine --------------
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

-------------- Parcial CraftMine --------------