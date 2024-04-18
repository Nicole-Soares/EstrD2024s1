-----Punto 3
module SetV2
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

 where


data Set a = S [a] Int deriving Show
            -- lista con repetidos, cantidad de elementos en la estructura
{-
    INV.REP.: en S xs n
            *si xs es vacío, n es cero
            *

-}
emptyS :: Set a -- Constante
addS :: Eq a => a -> Set a -> Set a -- Constante
belongs :: Eq a => a -> Set a -> Bool --Dados un elemento y un conjunto indica si el elemento pertenece al conjunto. / Lineal
sizeS :: Eq a => Set a -> Int --Devuelve la cantidad de elementos distintos de un conjunto. / constante
removeS :: Eq a => a -> Set a -> Set a -- Borra un elemento del conjunto  / lineal
unionS :: Eq a => Set a -> Set a -> Set a --Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos. Lineal
setToList :: Eq a => Set a -> [a] --Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto. LINEAL 


set1 = addS 8 (addS 5 (addS 4 emptyS))
set2= (addS 10 (addS 9 emptyS))
--implementacion de las operaciones

emptyS = S [] 0
---------------
addS a (S xs n) =   S (a : xs ) (n + 1) -- no se fija si ya estan en la lista
---------------------------------
belongs a (S xs _) = pertenece a xs
 ------------------   
--practica 2 
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y : ys) = x == y || pertenece x ys
-------------
sizeS (S _ n) = n
---------------------------
removeS y (S xs n) = S (listaSinElElemento xs y ) (n - cantidadDeVecesQueEstaElElemento y xs)
listaSinElElemento :: Eq a =>  [a] -> a -> [a]
listaSinElElemento [] _ = []
listaSinElElemento (x : xs) y = if x == y
                                then listaSinElElemento xs y
                                else x : listaSinElElemento xs y

cantidadDeVecesQueEstaElElemento :: Eq a => a -> [a] -> Int
cantidadDeVecesQueEstaElElemento x (y : ys) = unoSiCeroSiNo(x == y) + cantidadDeVecesQueEstaElElemento x ys

unoSiCeroSiNo :: Bool -> Int -- no se puede hacer pm en tipos abstractos 
unoSiCeroSino True = 1
unoSiCeroSiNo _ = 0
{-removeS y (S xs n) = S (listaSinElElemento xs y ) (n -1)
listaSinElElemento :: Eq a =>  [a] -> a -> [a]
listaSinElElemento [] _ = []
listaSinElElemento (x : xs) y = if x == y
                                then listaSinElElemento xs y
                                else x : listaSinElElemento xs y-}
--------------------
unionS  (S ls n) (S ls2 n2) = S (ls ++ ls2)  (length( ls ++ ls2))-- no me importa si hay repetidos



--------------------
setToList (S ls n) = sinRepetidos ls

sinRepetidos :: Eq a => [a] ->[a]
sinRepetidos [] = []
sinRepetidos (x : xs) = if pertenece x xs
                         then sinRepetidos xs
                         else x : sinRepetidos xs