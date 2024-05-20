data Color = Azul | Verde | Rojo deriving (Eq, Show)
data Torre = Base | Bloque Color Torre 
    deriving (Eq, Show)

torreEjemplo = Bloque Azul (Bloque Rojo Base)

todos ::  Color -> Torre-> Bool
 --Propósito: dados un color c y una torre, indica si todos los bloques de la torre son de color c.
todos c (Base) = True
todos c (Bloque ct t) = (c == ct) && todos c t

agregarOtrosN :: Int-> Color-> Torre-> Torre
 {-Propósito: dados un número n, un color c y una torre, agrega n bloques de cemento de color c a la torre, luego del primer
 bloque de color c.
 Precondición: hay al menos un bloque de color c.-}
agregarOtrosN n c Base = Base
agregarOtrosN n c (Bloque ct t) = if not (c == ct)
                                    then Bloque ct (agregarOtrosN n c t)
                                    else Bloque ct (agregarOtrosNT n c t)

agregarOtrosNT :: Int -> Color -> Torre -> Torre
agregarOtrosNT 0 _ t = t
agregarOtrosNT n c t = Bloque c (agregarOtrosNT (n - 1) c t)

sinColores :: [Color]-> Torre-> Torre
-- Propósito: dada una lista de colores y una torre, quita de la torre todos los bloques cuyo color sea alguno de los de la lista.
sinColores _ Base = Base
sinColores cs (Bloque c t) = if pertenece c cs
                                then sinColores cs t
                                else Bloque c (sinColores cs t)

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
--pertenece x (y : []) = x == y 
pertenece x (y : ys) = if x == y
                        then True
                        else pertenece x ys

aparicionesDeColores :: Torre-> [(Color,Int)]
 {-Propósito: dada una torre, denota la lista de pares donde el primer elemento es un color y el segundo elemento es la
 cantidad de veces que aparece dicho color.
 Nota: si el color no aparece, no hace falta que esté en la lista. -}
aparicionesDeColores Base = []
aparicionesDeColores (Bloque c t) = agregarOcurrencia c (aparicionesDeColores t)

agregarOcurrencia :: Color -> [(Color, Int)] -> [(Color, Int)]
agregarOcurrencia c [] = [(c, 1)]
agregarOcurrencia c ((ci, i): cis) = if ci == c 
                                      then (ci, i+1) : cis
                                      else (ci, i) : agregarOcurrencia c cis

----Escenarios
type Energia = Int
type Clave = Int
data Direccion = Izquierda | Centro | Derecha
data Dispositivo = Dispositivo Energia [Clave]
data Escenario = Acceso Clave
 | Pared Dispositivo
 | Punto Dispositivo Escenario Escenario Escenario

ejemploEscenario = Punto (Dispositivo 30 []) (Acceso 4321) (Pared (Dispositivo 40 [])) (Acceso 4321)                           

cantidadDeAccesos :: Escenario-> Int
-- Propósito: dado un escenario, denota la cantidad de accesos que contiene.
cantidadDeAccesos (Acceso _) = 1
cantidadDeAccesos (Pared _) = 0
cantidadDeAccesos (Punto _ e1 e2 e3) = cantidadDeAccesos e1 + cantidadDeAccesos e2 + cantidadDeAccesos e3


sePuedeSalirCon :: Energia-> Escenario-> Bool
 --Propósito: dada una cantidad de energía y un escenario, indica si es posible pasar de escenario con esa cantidad energía.
sePuedeSalirCon e (Acceso _) = True
sePuedeSalirCon e (Pared d) = e >= energia d
sePuedeSalirCon e (Punto d e1 e2 e3) = e >= (energia d + energiaDelEscenario e1 + energiaDelEscenario e2 + energiaDelEscenario e3)

energia :: Dispositivo -> Int
energia (Dispositivo e _) = e

energiaDelEscenario :: Escenario -> Int
energiaDelEscenario (Acceso _) = 0
energiaDelEscenario (Pared d) = energia d
energiaDelEscenario (Punto d e1 e2 e3) = energia d + energiaDelEscenario e1 + energiaDelEscenario e2 + energiaDelEscenario e3

caminoGanador :: [Direccion]-> Escenario-> Bool
{- Propósito: dado un camino y un escenario, indica si siguiendo el camino se pasa de escenario.
 Precondición: el camino es válido en el escenario.
 Nota: se asume suficiente cantidad de energía.-}

caminoGanador ds e = caminoGanadorD ds [] e

caminoGanadorD :: [Direccion] -> [Clave] -> Escenario -> Bool
caminoGanadorD [] cs (Acceso c) = pertenece c cs 
caminoGanadorD (di : dis) cs (Punto d e1 e2 e3) = let claves = cs ++ (claves d) in
                                                    if esIzquierda di
                                                        then caminoGanadorD dis claves e1
                                                        else if esDerecha di
                                                            then caminoGanadorD dis claves e3
                                                            else caminoGanadorD dis claves e2

claves :: Dispositivo -> [Clave]
claves 

esIzquierda :: Direccion -> Bool
esIzquierda d = d == Izquierda

esDerecha :: Direccion -> Bool
esDerecha d = d == Derecha
