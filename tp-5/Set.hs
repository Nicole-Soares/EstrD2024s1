module Set
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

 where

data Set a = S [a] Int deriving Show
            -- lista sin repetidos, cantidad de elementos en la estructura
{-
    INV.REP.: en S xs n
            *n es la longitud de xs
            *

-}
emptyS :: Set a --Crea un conjunto vacío. Constante
addS :: Eq a => a -> Set a -> Set a --  Lineal
belongs :: Eq a => a -> Set a -> Bool --Dados un elemento y un conjunto indica si el elemento pertenece al conjunto. Lineal
sizeS :: Eq a => Set a -> Int --Devuelve la cantidad de elementos distintos de un conjunto. Constante
removeS :: Eq a => a -> Set a -> Set a -- Borra un elemento del conjunto | Lineal
unionS :: Eq a => Set a -> Set a -> Set a --Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos. Lineal
setToList :: Eq a => Set a -> [a] --Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto. Constante

set1 = addS 8 (addS 5 (addS 4 emptyS))
set2= (addS 10 (addS 9 emptyS))
--implementacion de las operaciones
emptyS = S [] 0
---------------
addS a (S xs n) =  if pertenece a xs 
                    then S xs n 
                    else S (a : xs ) (n + 1)
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
removeS y (S xs n) = S (listaSinElElemento xs y ) (n -1)
listaSinElElemento :: Eq a =>  [a] -> a -> [a]
listaSinElElemento [] _ = []
listaSinElElemento (x : xs) y = if x == y
                                then xs
                                else x : listaSinElElemento xs y
--------------------
unionS  (S ls n) set = unionDeSets ls set

unionDeSets :: Eq a =>  [a] -> Set a -> Set a
unionDeSets [] set = set 
unionDeSets (x : xs)  set = unionDeSets xs (addS x set) -- me fijo si el elemento a agregar esta en el otro set

--------------------
setToList (S ls n) = ls



