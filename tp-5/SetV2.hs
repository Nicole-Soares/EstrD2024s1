-----Punto 3
module SetV2
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

 where


data Set a = S [a]  deriving Show
            -- lista con repetidos
{-
    INV.REP.: 
            --

-}
--Crea un conjunto vacío
--Constante
emptyS :: Set a 
emptyS = S []

--Dados un elemento y un conjunto, agrega el elemento al conjunto.
--Constante
addS :: Eq a => a -> Set a -> Set a 
addS x (S ls) = S (x:ls)

--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
--Lineal
belongs :: Eq a => a -> Set a -> Bool  
belongs x (S ls) = pertenece x ls
 
-----práctica 2
--en el peor de los casos es lineal (?)
pertenece :: a -> [a] -> Bool
pertenece x (y : ys) = x == y || pertenece x ys
----------------

--Devuelve la cantidad de elementos distintos de un conjunto. / constante
--Cuadratica
sizeS :: Eq a => Set a -> Int 
sizeS (S ls) = cantidadDeElementoNoRepetidos ls

cantidadDeElementosNoRepetidos :: [a] -> Int
cantidadDeElementosNoRepetidos [] = 0
cantidadDeElementosNoRepetidos (x : xs) = if pertenece x xs
                                            then cantidadDeElementosNoRepetidos xs
                                            else 1 + cantidadDeElementosNoRepetidos xs

-- Borra un elemento del conjunto  / lineal
removeS :: Eq a => a -> Set a -> Set a 
removeS x (S ls) = delete x ls

delete :: a -> [a] -> [a]
delete 
unionS :: Eq a => Set a -> Set a -> Set a --Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos. Lineal
setToList :: Eq a => Set a -> [a] --Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto. LINEAL 


set1 = addS 8 (addS 5 (addS 4 emptyS))
set2= (addS 10 (addS 9 emptyS))
--implementacion de las operaciones


---------------
addS a (S xs ) =   S (a : xs )  -- no se fija si ya estan en la lista
---------------------------------
belongs a (S xs ) = pertenece a xs
 ------------------   
--practica 2 
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y : ys) = x == y || pertenece x ys
-------------
sizeS (S xs ) = cantidadDeElementosNoRepetidos xs

cantidadDeElementosNoRepetidos :: [a] -> Int
cantidadDeElementosNoRepetidos [] =
cantidadDeElementosNoRepetidos (x:xs) = if pertenece x xs
                                            then cantidadDeElementosNoRepetidos xs
                                            else 1 cantidadDeElementosNoRepetidos xs
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