data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

pizza1= Capa Salsa pizza2
pizza2 = Capa (Aceitunas 3) pizza3
pizza3= Prepizza
pizza4 = Capa Queso pizza5
pizza5= Prepizza

--1 PIZZA

--1.1
cantidadDeCapas :: Pizza -> Int
--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

--1.2


--1.3
sacarJamon :: Pizza -> Pizza
--Le saca los ingredientes que sean jamón a la pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa i p) = if esJamon i 
                            then sacarJamon p
                            else Capa i (sacarJamon p)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False

--1.4
tieneSoloSalsaYQueso :: Pizza -> Bool
{-Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.) -}
tieneSoloSalsaYQueso Prepizza   = True
tieneSoloSalsaYQueso (Capa i p) = esSalsaOQueso i && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _ = False

--1.5
duplicarAceitunas :: Pizza -> Pizza
--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = Capa (duplicarSiEsAceituna i) (duplicarAceitunas p)

duplicarSiEsAceituna :: Ingrediente -> Ingrediente
duplicarSiEsAceituna i = if esAceituna i
                            then duplicar i
                            else i

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna _ = False

duplicar :: Ingrediente -> Ingrediente
duplicar (Aceitunas n) = Aceitunas (n * 2)

--1.6

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
{-Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
ingredientes de la pizza, y la respectiva pizza como segunda componente.-}
cantCapasPorPizza [] = []
cantCapasPorPizza (p : ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps

--2. MAPA DE TESOROS (con bifurcaciones)
{-Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
cada cofre tiene un objeto, que puede ser chatarra o un tesoro.
-}

data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show

mapa1 = Bifurcacion cofre1 mapa2 mapa3
cofre1 = Cofre [Tesoro, Tesoro]
cofre2= Cofre [Tesoro]
mapa2 = Fin cofre2
mapa3 = Bifurcacion cofre1 mapa4 mapa5

mapa4 = Fin cofre1
mapa5 = Fin cofre1
--2.1
hayTesoro :: Mapa -> Bool
--Indica si hay un tesoro en alguna parte del mapa.
hayTesoro (Fin c) = hayTesoroEnElCofre c
hayTesoro (Bifurcacion c izq der) = hayTesoroEnElCofre c || hayTesoro izq || hayTesoro der 

hayTesoroEnElCofre :: Cofre -> Bool
hayTesoroEnElCofre (Cofre objs) =  existeTesoroEntreLosObjetos objs
    
existeTesoroEntreLosObjetos :: [Objeto] -> Bool
existeTesoroEntreLosObjetos [] = False
existeTesoroEntreLosObjetos (obj : objs) = if not (esUnTesoro obj)
                                                then existeTesoroEntreLosObjetos objs
                                                else esUnTesoro obj
esUnTesoro :: Objeto -> Bool
esUnTesoro Tesoro = True
esUnTesoro _ = False


--2.2
hayTesoroEn :: [Dir] -> Mapa -> Bool
{-Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
lista vacía de direcciones.-}
hayTesoroEn [] (Bifurcacion c _ _) =  hayTesoroEnElCofre c
hayTesoroEn [] (Fin c) = hayTesoroEnElCofre c
hayTesoroEn (d : ds) (Bifurcacion _ izq der) = if esIzquierda d
                                                then hayTesoroEn ds izq
                                                else hayTesoroEn ds der

esIzquierda :: Dir -> Bool
esIzquierda Izq = True
esIzquierda _ = False


-- 2.3
caminoAlTesoro :: Mapa -> [Dir]
--Indica el camino al tesoro. 
--Precondición: existe un tesoro y es único.

caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c izq der) = if  hayTesoroEnElCofre c
                                         then []--singularSi([] hayTesoroEnElCofre c)
                                         else if hayTesoroHacia izq 
                                              then Izq : caminoAlTesoro izq 
                                              else Der : caminoAlTesoro der
                                        
hayTesoroHacia :: Mapa -> Bool 
hayTesoroHacia (Fin c) = hayTesoroEnElCofre c
hayTesoroHacia (Bifurcacion c izq der) = hayTesoroEnElCofre c || hayTesoroHacia izq || hayTesoroHacia der


{-consACadaElemento :: a -> [a] -> [a]
consACadaElemento x []       = []
consACadaElemento x (y:ys) = (x:ys) ++ consACadaElemento x ys
-}
--2.4
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
--Indica el camino de la rama más larga.
--si ninguna es mayor devuelve der porque asumo que es esa
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion c t1 t2) = if length (caminoDeLaRamaMasLarga t1) > length  (caminoDeLaRamaMasLarga  t2)
                                                then Izq : caminoDeLaRamaMasLarga t1
                                                else  Der : caminoDeLaRamaMasLarga  t2


--2.5
tesorosPorNivel :: Mapa -> [[Objeto]]
--Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel (Fin c) = [ cantidadDeTesoros c]
tesorosPorNivel (Bifurcacion c izq der) =  cantidadDeTesoros c : juntarlos (tesorosPorNivel izq) (tesorosPorNivel der)


cantidadDeTesoros :: Cofre -> [Objeto] -- hago recursion en la lista de objetos para ir contando los objetos que son tesoros
cantidadDeTesoros (Cofre objs) = cantidadDeTesorosEnLosObjetos objs

cantidadDeTesorosEnLosObjetos :: [Objeto] -> [Objeto]
cantidadDeTesorosEnLosObjetos [] = []
cantidadDeTesorosEnLosObjetos (obj : objs) = singularSi Tesoro (esUnTesoro obj) ++ cantidadDeTesorosEnLosObjetos objs

singularSi :: a -> Bool -> [a]
singularSi x True  = [x]
singularSi _ False = []

juntarlos :: [[a]] -> [[a]] -> [[a]]
juntarlos xss [] = xss
juntarlos [] yss = yss
juntarlos (xs : xss) ( ys : yss) = (xs ++ ys) : juntarlos xss yss -- junto en una misma lista los dos primeros elementos de la lista de listas

--2.6
todosLosCaminos :: Mapa -> [[Dir]]
--Devuelve todos lo caminos en el mapa.
todosLosCaminos (Fin _) = []
todosLosCaminos (Bifurcacion _ izq der) =  agregarDir Izq (todosLosCaminos izq) ++ agregarDir Der (todosLosCaminos der)

agregarDir :: Dir -> [[Dir]] -> [[Dir]]
agregarDir dir [] = [[dir]]
agregarDir dir (drs : []) =   [(dir : drs )] -- si queda un elemento o es solo un elemento, le agrego la dir a esa lista
agregarDir dir (drs : drss) =   (dir : drs ) : agregarDir dir drss 

--3 NAVE ESPACIAL

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Sector = S SectorId [Componente] [Tripulante] deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show

nave1 = N tree1
tree1 = NodeT sector1 tree2 tree3
tree2 = EmptyT
tree3 = EmptyT

sector1 = S "n" [LanzaTorpedos, Almacen[Comida, Oxigeno]] []

--3.1
sectores :: Nave -> [SectorId]
--Propósito: Devuelve todos los sectores de la nave.
sectores (N t) =  sectoresDelArbol t

sectoresDelArbol :: Tree Sector -> [SectorId]
sectoresDelArbol EmptyT = []
sectoresDelArbol (NodeT s izq der) = idDelSector s : sectoresDelArbol izq ++ sectoresDelArbol der

idDelSector :: Sector -> SectorId
idDelSector (S id _ _ ) = id

--3.2
poderDePropulsion :: Nave -> Int
{-Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
el poder de propulsión es el número que acompaña al constructor de motores.-}
poderDePropulsion (N t) = poderDePropulsionT t

poderDePropulsionT :: Tree Sector -> Int
poderDePropulsionT EmptyT = 0
poderDePropulsionT (NodeT s izq der)  = poderDePropulsionS s + poderDePropulsionT izq + poderDePropulsionT der

poderDePropulsionS :: Sector -> Int
poderDePropulsionS (S _ componentes _) = poderDePropulsionComponentes componentes

poderDePropulsionComponentes :: [Componente] -> Int
poderDePropulsionComponentes [] = 0
poderDePropulsionComponentes (c : cs) = propulsionDelComponente c +  poderDePropulsionComponentes cs

propulsionDelComponente :: Componente -> Int
propulsionDelComponente (Motor n) = n
propulsionDelComponente _ = 0

--3.3
barriles :: Nave -> [Barril]
--Propósito: Devuelve todos los barriles de la nave.
barriles (N t) = barrilesEnT t

barrilesEnT :: Tree Sector -> [Barril]
barrilesEnT EmptyT = []
barrilesEnT (NodeT s izq der) = barillesEnS s ++ barrilesEnT izq ++ barrilesEnT der

barillesEnS :: Sector -> [Barril]
barillesEnS (S _ componentes _) = barrilesEnComponentes componentes

barrilesEnComponentes :: [Componente] -> [Barril]
barrilesEnComponentes [] = []
barrilesEnComponentes (c : cs) =  barrilesEnComponente c ++ barrilesEnComponentes cs

barrilesEnComponente :: Componente -> [Barril]
barrilesEnComponente (Almacen bs) = bs
barrilesEnComponente _ = []

--3.4
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
{-Propósito: Añade una lista de componentes a un sector de la nave.
Nota: ese sector puede no existir, en cuyo caso no añade componentes.-}
agregarASector cs id (N t) = N (agregarASectorT cs id t)

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorT _ _ EmptyT = EmptyT
agregarASectorT cs id (NodeT s izq der) = if (idDelSector s) == id
                                          then NodeT (agregarASectorActual cs s) izq der
                                          else NodeT s (agregarASectorT cs id izq) (agregarASectorT cs id der)
{-
                                          if (n == y)
                                          then [(n, x + 1)] : yss 
                                          else [(n, x)] : recursion yss
-}

agregarASectorActual :: [Componente] -> Sector -> Sector
agregarASectorActual [] s = s
agregarASectorActual  cs1 (S id cs2 ts) = S id (cs1 ++ cs2) ts 

--3.5
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA t ids (N tree) = N (asignarTripulanteATree t ids tree)

asignarTripulanteATree :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteATree _ _ EmptyT = EmptyT
asignarTripulanteATree t ids (NodeT s izq der) =  NodeT (asignarTripulanteASectores t ids s) (asignarTripulanteATree t ids izq) (asignarTripulanteATree t ids der)

asignarTripulanteASectores :: Tripulante -> [SectorId] -> Sector -> Sector
asignarTripulanteASectores _ [] s = s
asignarTripulanteASectores t (id : ids) s = if id == idDelSector s
                                                then asignarTripulanteASectorActual t s
                                                else asignarTripulanteASectores t ids s

asignarTripulanteASectorActual :: Tripulante -> Sector -> Sector
asignarTripulanteASectorActual t (S id comps tps) = S id comps (t : tps)

--3.6
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
--Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados t (N tree) = sectoresAsignadosTree t tree

sectoresAsignadosTree :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosTree _ EmptyT = []
sectoresAsignadosTree t (NodeT s izq der) = if elSectorTieneAlTripulante s t 
                                                then idDelSector s : sectoresAsignadosTree t izq ++ sectoresAsignadosTree t der
                                                else sectoresAsignadosTree t izq ++ sectoresAsignadosTree t der

elSectorTieneAlTripulante :: Sector -> Tripulante -> Bool
elSectorTieneAlTripulante (S id _ ts) t = laListaDeTripulantesContiene ts t


laListaDeTripulantesContiene :: [Tripulante] -> Tripulante -> Bool
laListaDeTripulantesContiene (t : ts) t2 = t == t2 

--3.7
tripulantes :: Nave -> [Tripulante]
--Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes (N t) = sinRepetidos(tripulantesDelTree t)

tripulantesDelTree :: Tree Sector -> [Tripulante]
tripulantesDelTree EmptyT = []
tripulantesDelTree (NodeT s izq der) = tripulantesDelSector s ++ tripulantesDelTree izq ++ tripulantesDelTree der

tripulantesDelSector :: Sector -> [Tripulante]
tripulantesDelSector (S _ _ tps) = tps

sinRepetidos :: [Tripulante] -> [Tripulante]
sinRepetidos [] = []
sinRepetidos (t : tps) = if contieneElTripulante t (sinRepetidos tps)
                            then sinRepetidos tps
                            else t : sinRepetidos tps

contieneElTripulante :: Tripulante -> [Tripulante] -> Bool
contieneElTripulante t [] = False
contieneElTripulante t (t2 :tps) = t == t2 || contieneElTripulante t tps

--4 MANADA DE LOBOS

{-Modelaremos una manada de lobos, como un tipo Manada, que es un simple registro compuesto
de una estructura llamada Lobo, que representa una jerarquía entre estos animales.
Los diferentes casos de lobos que forman la jerarquía son los siguientes:
Los cazadores poseen nombre, una lista de especies de presas cazadas y 3 lobos a cargo.
Los exploradores poseen nombre, una lista de nombres de territorio explorado (nombres de
bosques, ríos, etc.), y poseen 2 lobos a cargo.
Las crías poseen sólo un nombre y no poseen lobos a cargo.
La estructura es la siguiente:-}
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
data Manada = M Lobo

--4.1. 
{-Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
que corresponda en cada caso:-}

manada = M lobo_cazador
lobo_cazador = Cazador "Carlitos" presas lobo_explorador1 lobo_explorador2 lobo_cazador2
presas = ["Bambi", "Simba"]

lobo_cazador2 = Cazador "Pepita" presas lobo_cria1 lobo_cria2 lobo_cria3
lobo_explorador1= Explorador "Pipi" territios1 lobo_cria2 lobo_cria3
lobo_explorador2= Explorador "Cucu" territios2 lobo_cria2 lobo_cria3

territios1 = ["Buenos Aires", "Francia"]
territios2 = ["Grecia", "Italia"]
lobo_cria1 = Cria "Simba"

lobo_cria2 = Cria "Dumbo"

lobo_cria3 = Cria "Sebastian"
--4.2
buenaCaza :: Manada -> Bool
--Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza m = cantidadDeAlimento m > cantidadDeCrias m

cantidadDeAlimento :: Manada -> Int
cantidadDeAlimento (M l) = cantidadDeAlimentoL l

cantidadDeAlimentoL :: Lobo -> Int
cantidadDeAlimentoL (Cria _) = 0
cantidadDeAlimentoL (Cazador _ ps l1 l2 l3) = cantidadDeAlimentoP ps + cantidadDeAlimentoL l1 + cantidadDeAlimentoL l2 + cantidadDeAlimentoL l3
cantidadDeAlimentoL (Explorador _ _ l1 l2) =  cantidadDeAlimentoL l1 + cantidadDeAlimentoL l2 

cantidadDeAlimentoP :: [Presa] -> Int
cantidadDeAlimentoP [] = 0
cantidadDeAlimentoP (p : ps) = 1 + cantidadDeAlimentoP ps

cantidadDeCrias :: Manada -> Int
cantidadDeCrias (M l) = cantidadDeCriasL l

cantidadDeCriasL :: Lobo -> Int
cantidadDeCriasL (Cria _) = 1
cantidadDeCriasL (Cazador _ ps l1 l2 l3) =  cantidadDeCriasL l1 + cantidadDeCriasL l2 + cantidadDeCriasL l3
cantidadDeCriasL (Explorador _ _ l1 l2) =  cantidadDeCriasL l1 + cantidadDeCriasL l2 

--4.3
elAlfa :: Manada -> (Nombre, Int)
{-Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
cero presas.-}

elAlfa (M l) = elAlfaL l

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cria n) = (n, 0)
elAlfaL (Cazador n ps l1 l2 l3) = elegirEntre (n, (cantidadDeAlimentoP ps)) (elegirEntre (elAlfaL l1) (elegirEntre (elAlfaL l2) (elAlfaL l3)))
elAlfaL (Explorador n _ l1 l2) =  elegirEntre (elAlfaL l1) (elegirEntre (elAlfaL l2) (n, 0))

elegirEntre :: (Nombre , Int) -> (Nombre, Int) -> (Nombre, Int)
elegirEntre (n , cantidad) (n2, cantidad2) = if cantidad >= cantidad2
                                                  then (n , cantidad)
                                                  else (n2 , cantidad2) 

--4.4
losQueExploraron :: Territorio -> Manada -> [Nombre]
{-Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
pasaron por dicho territorio.-}
losQueExploraron t (M l) = losQueExploraronLobo t l

losQueExploraronLobo :: Territorio -> Lobo -> [Nombre]
losQueExploraronLobo _ (Cria _) = [] 
losQueExploraronLobo t (Cazador _ _ l1 l2 l3) = losQueExploraronLobo t l1 ++ losQueExploraronLobo t l2 ++ losQueExploraronLobo t l3
losQueExploraronLobo t (Explorador n ts l1 l2) =  singularSi n (territoriosExploradosPorElLoboContienenA t ts) ++ losQueExploraronLobo t l1 ++ losQueExploraronLobo t l2

territoriosExploradosPorElLoboContienenA :: Territorio -> [Territorio] -> Bool
territoriosExploradosPorElLoboContienenA t (t1 : t1s) = t == t1 || territoriosExploradosPorElLoboContienenA t t1s


--4.5
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
{-Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
dicho territorio. Los territorios no deben repetirse.-}
exploradoresPorTerritorio (M l) = exploradoresPorTerritorioLobo l

exploradoresPorTerritorioLobo :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioLobo (Cria _) = [] 
exploradoresPorTerritorioLobo (Cazador _ _ l1 l2 l3) = exploradoresPorTerritorioLobo l1 ++ exploradoresPorTerritorioLobo l2 ++ exploradoresPorTerritorioLobo l3
exploradoresPorTerritorioLobo (Explorador n ts l1 l2) = agregar n ts (exploradoresPorTerritorioLobo l1 ++ exploradoresPorTerritorioLobo l2)

agregar :: Nombre -> [Territorio]-> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] 
agregar n ts [] = agregarTerritoriosYNombre n ts
agregar n (t : ts) ((tt , n2s) : listatuplas) = if t == tt
                                                  then (tt , (n : n2s)) : agregar n ts listatuplas
                                                  else agregar n ts listatuplas


agregarTerritoriosYNombre :: Nombre -> [Territorio] -> [(Territorio, [Nombre])]
agregarTerritoriosYNombre _ [] = []
agregarTerritoriosYNombre n (t : ts) =  (t , [n]): agregarTerritoriosYNombre n ts

--4.6
superioresDelCazador :: Nombre -> Manada -> [Nombre]
{-Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los
cazadores que tienen como subordinado al cazador dado (directa o indirectamente).
Precondición: hay un cazador con dicho nombre y es único.-}
superioresDelCazador n (M l) = superioresDelCazadorLobo n l

superioresDelCazadorLobo :: Nombre -> Lobo -> [Nombre]
superioresDelCazadorLobo _ (Cria _) = [] 
superioresDelCazadorLobo n (Cazador nc _ l1 l2 l3) = if cazardorEsSuperiorDe l1 n || cazardorEsSuperiorDe l2 n || cazardorEsSuperiorDe l3 n
                                                       then nc :  superioresDelCazadorLobo n l1 ++ superioresDelCazadorLobo n l2 ++ superioresDelCazadorLobo n l3
                                                       else superioresDelCazadorLobo n l1 ++ superioresDelCazadorLobo n l2 ++ superioresDelCazadorLobo n l3
superioresDelCazadorLobo n (Explorador ne ts l1 l2) = superioresDelCazadorLobo n l1 ++ superioresDelCazadorLobo n l2 

cazardorEsSuperiorDe :: Lobo  -> Nombre -> Bool
cazardorEsSuperiorDe (Cria nc) n = nc == n
cazardorEsSuperiorDe (Cazador nc _ _ _ _) n = nc == n
cazardorEsSuperiorDe (Explorador ne _ _ _ ) n = ne == n






