-----Punto 3
module SetV2
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

 where


data Set a = S [a]  deriving Show
            -- lista con repetidos
{-

INV.REP.: = -
-}

set1 = addS 8 (addS 5 (addS 4 emptyS))
set2= (addS 10 (addS 9 emptyS))


--Crea un conjunto vacío
--Constante:
--O (1 ya que siempre retorna lo mismo que es un Set con lista vacía)
--O(1)
emptyS :: Set a 
emptyS = S []

--Dados un elemento y un conjunto, agrega el elemento al conjunto.
--Constante:
--O (1 ya que siempre retorna lo mismo con un elemento agregado a la lista)
--O(1)
addS :: Eq a => a -> Set a -> Set a 
addS x (S ls) = S (x:ls)

--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
--Lineal:
-- O (n sobre la longitud de la lista ls que hace recursion en la subtarea)
--O(n)
belongs :: Eq a => a -> Set a -> Bool  
belongs x (S ls) = pertenece x ls
 
-----práctica 2
--en el peor de los casos es lineal (?)
pertenece :: a -> [a] -> Bool
pertenece x (y : ys) = x == y || pertenece x ys
----------------

--Devuelve la cantidad de elementos distintos de un conjunto. 
--Cuadratica:
--O(n dado el largo de la lista que hace recursion sobre si misma)
-- * n dado que pertenece es  O(n) con n siendo el largo de xs )
-- O(n * n)
-- O (n^2)
sizeS :: Eq a => Set a -> Int 
sizeS (S ls) = cantidadDeElementosNoRepetidos ls

cantidadDeElementosNoRepetidos :: [a] -> Int
cantidadDeElementosNoRepetidos [] = 0
cantidadDeElementosNoRepetidos (x : xs) = if pertenece x xs
                                            then cantidadDeElementosNoRepetidos xs
                                            else 1 + cantidadDeElementosNoRepetidos xs

-- Borra un elemento del conjunto 
--Lineal:
--O(n siendo la longitud de la lista ls, porque se hace un recursion sobre la misma)
--O(n)
removeS :: Eq a => a -> Set a -> Set a 
removeS x (S ls) = S (delete x ls)

delete :: a -> [a] -> [a]
delete _ [] = []
delete x (y : ys) = if x == y
                       then ys 
                       else y : delete x ys

--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
--Lineal:                                                           
--O( n siendo la longitud de la lista ls, porque se hace recursion sobre la misma           || O(n porque agregar es lineal)
-- *                                                                                            O(n)
-- 1 debido a que adds ces constante, agregando un elemento a la lista)
--O(n * 1)
-- O(n)
unionS :: Eq a => Set a -> Set a -> Set a 
unionS (S ls) set = agregar ls set

agregar :: [a] -> Set a -> Set a
agregar [] set = set
agregar (x : xs) set = addS x ( agregar xs set)

--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto. 
--Cuadratica
--O ( n siendo la longitud de la lista ls, porque se hace recursion sobre la lista
-- *
-- n dado que pertenece es O(n), y vuelve a recorrer por cada elemento)
-- O (n*n)
--O n^2 
setToList :: Eq a => Set a -> [a] 
setToList (S ls) = setToListS ls

setToListS :: [a] -> [a]
setToListS [] = []
setToListS (x : xs) = if pertenece x xs
                        then setToListS xs
                        else x : setToListS xs


