--1.1 TIPOS RECURSIVOS SIMPLES
--Celdas con bolitas
data Color = Azul | Rojo 
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

celda1 = Bolita Azul celda2
celda2 = Bolita Azul celda3
celda3 = CeldaVacia

--1.1.a

nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas c1 (Bolita c2 celda) = unoSiCeroSino (mismoColor c1 c2) + nroBolitas c1 celda

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _= 0

mismoColor :: Color -> Color -> Bool
mismoColor Rojo Rojo = True
mismoColor Azul Azul = True
mismoColor _ _ = False

--1.1.b

poner :: Color -> Celda -> Celda
poner c1 celda = Bolita c1 celda

--1.1.c

sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar c1 (Bolita c2 celda) = if mismoColor c1 c2
                                then celda
                                else Bolita c2 (sacar c1 celda) -- si no son del mismo color, "pongo" la celda actual que no es del color que busco

--1.1.c

--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ c = c -- devuelvo la misma celda que me dieron como argumento
ponerN n c celda = poner c (ponerN ( n - 1) c celda)  

--1.2  CAMINO HACIA EL TESORO

data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

camino1 = Cofre listaObje1 camino4
camino2 = Nada camino4
camino3= Fin
camino4= Cofre listaObje2 camino5
camino5 = Cofre listaObje3 camino6
camino6= Fin
listaObje1 = [Cacharro, Cacharro]
listaObje2 = [Tesoro, Tesoro]
listaObje3 = [Cacharro, Cacharro, Cacharro]

--1.2.a

--Indica si hay un cofre con un tesoro en el camino
hayTesoro :: Camino -> Bool
hayTesoro Fin = False --caso base
hayTesoro (Nada c) =  hayTesoro c   --caso base
hayTesoro (Cofre o c) = hayTesoroEnLosObjetos o || hayTesoro c -- caso recursivo chequeo la lista de objetos del cofre actual o me fijo en el resto del camino

hayTesoroEnLosObjetos :: [Objeto] -> Bool
hayTesoroEnLosObjetos [] = False
hayTesoroEnLosObjetos ( o : obs) = esUnTesoro o || hayTesoroEnLosObjetos obs

esUnTesoro :: Objeto -> Bool
esUnTesoro Tesoro = True
esUnTesoro _ = False

--1.2.b

{-
Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
Si un cofre con un tesoro está al principio del camino,  la cantidad de pasos a recorrer es 0.
Precondición: tiene que haber al menos un tesoro.
-}

pasosHastaTesoro :: Camino -> Int --preguntar caso precond
--Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro Fin = error "Tiene que haber al menos un tesoro"
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre objs c) = if hayTesoroEnLosObjetos objs
                                     then 0
                                     else  1 + pasosHastaTesoro c   
    

--1.2.c

{- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
pasos es 5, indica si hay un tesoro en 5 pasos.
-}

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n Fin = False
hayTesoroEn n c = buscarTesoro n  c

buscarTesoro :: Int -> Camino -> Bool
buscarTesoro _ Fin = False
buscarTesoro n1 (Nada c) = if n1 == 0
                            then False
                            else buscarTesoro (n1 - 1) c
buscarTesoro n1 (Cofre objs c) = if n1 == 0
                                        then hayTesoroEnLosObjetos objs
                                        else buscarTesoro (n1 - 1) c



--1.2.d

--Indica si hay al menos "n" tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n c = mismaCantidadDeTesoros n (cantidadTotalDeTesorosDelCamino c) -- comparo si la cantidad dada por parámetro es igual al total de tesoros que tiene ese camino

mismaCantidadDeTesoros :: Int -> Int -> Bool
mismaCantidadDeTesoros n1 n2 = n1 == n2

cantidadTotalDeTesorosDelCamino :: Camino -> Int -- calculo la cantidad total de tesoros del camino
cantidadTotalDeTesorosDelCamino Fin = 0
cantidadTotalDeTesorosDelCamino (Nada c) = 0 + cantidadTotalDeTesorosDelCamino c
cantidadTotalDeTesorosDelCamino (Cofre objs c) = cantidadDeTesoros objs +  cantidadTotalDeTesorosDelCamino c  

cantidadDeTesoros :: [Objeto] -> Int -- hago recursion en la lista de objetos para ir contando los objetos que son tesoros
cantidadDeTesoros [] = 0
cantidadDeTesoros (ob : objs) = unoSiCeroSino (esUnTesoro ob) + cantidadDeTesoros objs


--1.2.e
camino7 = Nada camino8
camino8= Nada camino9
camino9 = Nada camino10
camino10= Cofre listaObje1 camino11
camino11= Fin
{-
Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
incluidos tanto 3 como 5 en el resultado.
-}
cantTesorosEntre :: Int -> Int -> Camino -> Int                                              --Camino ( Nada (Nada (Nada (Cofre([tesoro] Fin))))) 1 2
--PRECOND: j (el segundo número pasado) tiene que ser >= a i (el primer número pasado)
                                                                                            -- Camino (Nada (Cofre  [Tesoro], Fin)) 0,1
--llegar hasta el minimo de los pasos y despues contar los tesoros hasta al proximo rango
cantTesorosEntre _ _ Fin              = 0 --si no hay camino no hay tesoro
cantTesorosEntre n1 n2 (Nada c)       =  if n1 == 0 -- si ya llego al minimo rango (x; _) 
                                            then 0 + contarTesorosHastElMaxDelRango c n2 -- entonces  me encargo de contar los tesoros (sin contar los tesoros del camino actual porque no tiene) hasta llegar al max rango
                                            else cantTesorosEntre (n1 - 1) (n2 - 1) c --sino sigo pasando por los caminos, restando los dos rangos para sacar los pasos a hacer entre esos dos numeros
cantTesorosEntre n1 n2 (Cofre objs c) = if n1 == 0
                                        then cantidadDeTesoros objs + contarTesorosHastElMaxDelRango c n2
                                        else cantTesorosEntre (n1 - 1) (n2 - 1) c



contarTesorosHastElMaxDelRango :: Camino -> Int -> Int
contarTesorosHastElMaxDelRango _ 0 = 0
contarTesorosHastElMaxDelRango Fin _ = 0
contarTesorosHastElMaxDelRango (Nada c) n = contarTesorosHastElMaxDelRango c (n - 1)
contarTesorosHastElMaxDelRango (Cofre objs c) n =  cantidadDeTesoros objs + contarTesorosHastElMaxDelRango c (n - 1)


--2. TIPOS ARBÓREOS

--2.1. Árboles binarios

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show


tree1 :: Tree Int
tree1 = NodeT 1 tree2 tree3 -- preguntar porque no lo toma si esta asi

tree2 :: Tree Int
tree2 = NodeT 2 tree4 tree5

tree3 :: Tree Int
tree3 = NodeT 3 tree6 tree7

tree4 :: Tree Int
tree4 = NodeT 4 EmptyT EmptyT

tree5 :: Tree Int
tree5 = NodeT 5 EmptyT EmptyT

tree6:: Tree Int
tree6 = NodeT 6 EmptyT EmptyT

tree7:: Tree Int
tree7 = NodeT 7 EmptyT EmptyT

tree8:: Tree Int
tree8 = NodeT 8 EmptyT EmptyT



--2.1.1
sumarT :: Tree Int -> Int
-- Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT EmptyT = 0
sumarT (NodeT n arbol1 arbol2) = n + sumarT arbol1 + sumarT arbol2

--2.1.2
sizeT :: Tree a -> Int
--Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
sizeT EmptyT = 0
sizeT (NodeT n arbol1 arbol2) = 1 + sizeT arbol1 + sizeT arbol2

--2.1.3
mapDobleT :: Tree Int -> Tree Int
--Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x arbol1 arbol2) = NodeT (x * 2) (mapDobleT arbol1) (mapDobleT arbol2)

--2.1.4
perteneceT :: Eq a => a -> Tree a -> Bool
--Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en elárbol.
perteneceT _ EmptyT = False
perteneceT x (NodeT y arbol1 arbol2) = mismoElemento x y || perteneceT x arbol1 || perteneceT x arbol2


mismoElemento :: Eq a => a -> a -> Bool
mismoElemento x y = x == y

--2.1.5
aparicionesT :: Eq a => a -> Tree a -> Int
--Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
aparicionesT _ EmptyT = 0
aparicionesT x (NodeT y arbol1 arbol2) = unoSiCeroSino (x == y) + aparicionesT x arbol1 + aparicionesT x arbol2

--2.1.6
leaves :: Tree a -> [a]
--Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves EmptyT = []
leaves (NodeT x arbol1 arbol2) = x : leaves arbol1 ++ leaves arbol2

--2.1.7
heightT :: Tree a -> Int
{-Dado un árbol devuelve su altura.
Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
de niveles del árbol1
. La altura para EmptyT es 0, y para una hoja es 1.
-}
heightT EmptyT = 0
heightT (NodeT _ arbol1 arbol2) = 1 + heightT arbol1 + heightT arbol2

--2.1.8
mirrorT :: Tree a -> Tree a
--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del árbol.
mirrorT EmptyT = EmptyT
mirrorT (NodeT x arbol1 arbol2) = NodeT x (mirrorT arbol2) (mirrorT arbol1) 

--2.1.9
toList :: Tree a -> [a]
{-Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
y luego los elementos del hijo derecho.-}
toList EmptyT = []
toList (NodeT x arbol1 arbol2) = toList arbol1 ++ (x : []) ++ toList arbol2

--2.1.10
levelN :: Int -> Tree a -> [a]
{-Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
distancia de la raiz a uno de sus hijos es 1.
Nota: El primer nivel de un árbol (su raíz) es 0.-}
levelN _ EmptyT = []
levelN n (NodeT x arbol1 arbol2) = if n == 0
                                    then x : []
                                    else levelN (n - 1) arbol1 ++ levelN (n - 1) arbol2


--2.1.11

listPerLevel :: Tree a -> [[a]]
--Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel EmptyT = []
listPerLevel (NodeT x n1 n2) = [x] : juntarNiveles (listPerLevel n1) (listPerLevel n2)


juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles []    xss = xss
juntarNiveles xss   []  = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

--2.1.12

ramaMasLarga :: Tree a -> [a]
--Devuelve los elementos de la rama más larga del árbol
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT _ ramaizq ramader) = elementosDelArbol(laRamaMasLarga ramaizq ramader)

laRamaMasLarga :: Tree a -> Tree a -> Tree a
laRamaMasLarga arbol1  arbol2 = if heightT arbol1 > heightT arbol2
                                    then arbol1
                                    else arbol2

elementosDelArbol :: Tree a -> [a]
elementosDelArbol EmptyT = []
elementosDelArbol (NodeT x ramaizq ramader) = x : elementosDelArbol ramaizq ++ elementosDelArbol ramader

--2.1.13

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) = [[x]] ++ restoDelCamino x (todosLosCaminos t1) ++ restoDelCamino x (todosLosCaminos t2)

restoDelCamino :: a -> [[a]] -> [[a]]
restoDelCamino x [] = []
restoDelCamino x (ls: lss) = (x : ls) : restoDelCamino x lss
--                  [a, b]      [[]]
{-
aux 4 ([5]: []) = (4 : [5]) : [[]]
                    [4, 5]  -> [[4, 5]]
-}

--2.2. EXPRESIONES ARITMÉTICAS

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA


--2.2.1
eval :: ExpA -> Int
--Dada una expresión aritmética devuelve el resultado evaluarla.
eval (Valor n) = n
eval (Sum expa1 expa2) = eval expa1 + eval expa2
eval (Prod expa1 expa2) = eval expa1 * eval expa2
eval (Neg expa) = - eval expa

--2.2.2
--2.2.a
simplificar :: ExpA -> ExpA
--Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando notación matemática convencional):
-- 0 + x = x + 0 = x
simplificar (Sum (Valor 0) e) = simplificar e
simplificar (Sum e (Valor 0)) = simplificar e
simplificar e = e

--2.2.b
-- 0 * x = x * 0 = 0
simplificar2 :: ExpA -> ExpA
simplificar2 (Prod (Valor 0) e) = simplificar e
simplificar2 (Prod e (Valor 0)) = simplificar e
simplificar2 e = e

--2.2.c
--1 * x = x * 1 = x
simplificar3 :: ExpA -> ExpA
simplificar3 (Prod (Valor 1) e ) = simplificar3 e
simplificar3 (Prod e (Valor 1))  = simplificar3 e
simplificar3 e = e


--2.2.d
-- - (- x) = x
simplificar4 :: ExpA -> ExpA
simplificar4 (Neg(Neg(Valor e))) = Valor e