module Set
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

 where

data Set a = S [a] Int deriving Show
            -- lista sin repetidos, cantidad de elementos en la estructura
{-
    INV.REP.: en S xs n

             * xs no tiene repetidos 
            * n es la longitud de la lista
           
-}

set1 = addS 8 (addS 5 (addS 4 emptyS))
set2 = (addS 10 (addS 9 emptyS))

--Implementacion----------------------------------------------

--Crea un conjunto vacío. 
--Constante:
--O(1)
emptyS :: Set a 
emptyS = S [] 0

---Dados un elemento y un conjunto, agrega el elemento al conjunto.
--Lineal:
--O(n donde n representa a pertenece la cual es O(n))
--O(n)
addS :: Eq a => a -> Set a -> Set a 
addS a (S xs n) =  if pertenece a xs 
                    then S xs n 
                    else S (a : xs ) (n + 1)

--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
--Lineal
belongs :: Eq a => a -> Set a -> Bool 
belongs a (S xs _) = pertenece a xs

--Devuelve la cantidad de elementos distintos de un conjunto. 
--Constante
--O(1)
sizeS :: Eq a => Set a -> Int 
sizeS (S _ n) = n

-- Borra un elemento del conjunto 
--O(n)
removeS :: Eq a => a -> Set a -> Set a 
removeS y (S xs n) = S (listaSinElElemento xs y ) (n -1)

listaSinElElemento :: Eq a =>  [a] -> a -> [a]
listaSinElElemento [] _ = []
listaSinElElemento (x : xs) y = if x == y
                                then xs
                                else x : listaSinElElemento xs y

--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos. 
--Cuadratica
unionS :: Eq a => Set a -> Set a -> Set a 
unionS  (S ls n) set = unionDeSets ls set

unionDeSets :: Eq a =>  [a] -> Set a -> Set a
unionDeSets [] set = set 
unionDeSets (x : xs)  set = unionDeSets xs (addS x set) -- me fijo si el elemento a agregar esta en el otro set

--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto. 
-- Constante
setToList :: Eq a => Set a -> [a] 
setToList (S ls n) = ls






 
--practica 2 
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y : ys) = x == y || pertenece x ys





bajaDeTripulante :: Tripulante -> Nave -> Nave
bajaDeTripulante t n = naveSinTripulante t n (sectores n) (naveVacia (sectores n))


naveSinTripulante :: Tripulante -> Nave -> [Sector] -> Nave -> Nave
naveSinTripulante _ _  [] nnueva = nnueva
-- Por cada sector, voy a agregar cada uno de los tripulantes que no sean t
naveSinTripulante t nvieja (s:ss) nnueva = naveSinTripulante t nvieja ss (agregarTripulantesAlSector t (set2list (tripulantesDe s nvieja)) s nnueva)
    
    agregarTripulantesAlSector 

-- Trabajo sobre un solo sector
-- Traigo el Set Tripulantes y chequeo que no exista el tripulante actual
-- Retorno la nave con el sector con todos sus tripulantes
agregarTripulantesAlSector :: Eq Tripulante => Tripulante -> [Tripulante] -> Sector -> Nave -> Nave
agregarTripulantesAlSector t [] sector nave = nave
agregarTripulantesAlSector t (trip:trips) sector nave = if t == trip
                                                        then agregarTripulantesAlSector t trips sector nave
                                                        else agregarTripulantesAlSector t trips sector (agregarTripulante trip sector nave)




