module MultiSet --(multiconjunto)
    (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS)
 where

import Map


data MultiSet a = MS (Map a Int ) deriving Show
--una lista donde tenga clave (el elemento), valor (la cantidad de apariciones del elemento)
{-
    INV.REP.: (MS map) 
    no hay claves repetidas en map
-}

multiSet1 = map1
map1 = addMS 8 emptyMS
map2 = emptyM

multiSet2 = map3
map3 = addMS 5 emptyMS

--Implementacion

--Propósito: denota un multiconjunto vacío.
--O(1)
emptyMS :: MultiSet a
emptyMS = MS emptyM


{-Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
multiconjunto.-}
--O(n # lineal ya que su subtarea (sumarUnoAlValorDeLaClave) es lineal)
--O(n)
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS k (MS m) = MS (sumarUnoAlValorDeLaClave k m)

--O(n # ya que su subtarea (assocM) es lineal)
--O(n)
sumarUnoAlValorDeLaClave :: Eq k => k -> Map k Int -> Map k Int 
sumarUnoAlValorDeLaClave k m = case lookupM k m of
                                    Just v  -> assocM k (v + 1) m
                                    Nothing -> assocM k  1 m


{-Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
elemento en el multiconjunto.-}
--O(n # ya que la subtarea (lookupM) es lineal)
--O(n)
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS k (MS map) = case lookupM k map of
                                Just v -> v
                                Nothing -> 0

{-
Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
ambos multiconjuntos.
-}
--
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a --(opcional)
unionMS (MS map1) (MS map2) = MS(merge map1 map2)

--
merge ::Eq a => Map a Int -> Map a Int -> Map a Int
merge map1 map2 = mergeClaves (keys map1) map1 map2

--O(n #es la longitud de la lista [a] que se hace recursion sobre la misma
--  *
--  m # es la longitud de claves (k) de map2 (lookupM es lineal) 
--  +
--  m' # es la longitud de claves (k) de map1 (lookupM es lineal)    
--  +
--  1 # la subtarea unionDeValores' es constante
--  +
--  v # es la longitud de claves (k) del map generado por la recursion (mergeClaves))
--O(n * (m + m' + v))
--O(n^2)
mergeClaves ::Eq a => [a] -> Map a Int  -> Map a Int -> Map a Int
mergeClaves [] _ map2 = map2
mergeClaves (k:ks) map1 map2 = assocM k (unionDeValores' (lookupM k map1) (lookupM k map2)) (mergeClaves ks map1 map2)

{-unir :: [k] -> Map a Int -> MultiSet a -> Map a Int -- recursion sobre las claves del map 1, despues uso assocM pasandole la clave y el valor de la clave actual con el valor que tiene en el map 2, si no tiene clave retornara nothing
unir [] _ (MS map2) = map2
unir (k : ks) map1 multiSet = assocM k (unionDeValores (lookupM k map1) (ocurrencesMS k multiSet)) (unir ks map1 map2)-}

{-unionDeValores :: Maybe Int -> Int -> Int
unionDeValores (Just n1) n2 = n1 + n2    
unionDeValores _ n2 = n2               
-}
--O(1)
unionDeValores' :: Maybe Int -> Maybe Int -> Int
unionDeValores' (Just v1) (Just v2) = v1 + v2
unionDeValores' Nothing (Just v) =  v
unionDeValores' (Just v) Nothing =  v
unionDeValores' _ _ = 0


{-
Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
multiconjuntos tienen en común.
-}
--O(n #por el tiempo de ejecucion de la funcion keys sobre map1
-- +
--(n * (m + m + n + m + 1 + n)) #por la subatarea iguales sobre map1 y map2)
-- O(n + (n * (m + m + n + m + 1 + n)))
--O (n + n( * (3m + 2n)))
--O (n + 3nm + 2n^2)
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a --(opcional)
intersectionMS (MS map1) (MS map2) = MS(iguales (keys map1) map1 map2)


--O(n #es la longitud de la lista [a] que se hace recursion sobre la misma
-- * por cada elemento de [a] se hace una recursión sobre las claves de map2
-- (
--    m # por el tiempo de ejecucion de la funcion keys sobre map2
--     +
--    m # por el tiempo de ejecucion de la funcion pertenece, que recorre la lista de claves de map2
--     +
--    n por el lookupM sobre map1
--     +
--    m por el lookupM sobre map2
--     +
--    1 por la funcion unionDeValores' que es constante
--    +
--    n por la funcion assocM que es lineal sobre ks1
-- )
-- O(n * (m + m + n + m + 1 + n))
-- O(n * (3m + 2n +1))
-- O(3nm + 2n^2 + n)
-- O(nm + n^2)


-- O(n*m), con m <= n -> O(n^2), porque en el peor de los casos m == n
-- O(n*m), con m > n  -> O(n*m)
iguales :: Eq a => [a] -> Map a Int  -> Map a Int -> Map a Int
iguales [] _ map2 = emptyM             -- O(2m)
iguales (k : ks1) map1 map2 = if pertenece k (keys map2) -- +
                                    --   O(n)     +     O(1)      +      O(n)     +        O(m)
                                    then assocM k (unionDeValores' (lookupM k map1) (lookupM k map2)) (iguales ks1 map1 map2) -- si pertenece lo asociamos
                                    else iguales ks1 map1 map2 

                                -- n -> {1, 2, 3}
                                -- m -> {1, 2}
                                -- 


-----práctica 2
--en el peor de los casos es lineal (?)
pertenece :: Eq k => k -> [k] -> Bool
pertenece _ [] = False
pertenece k (k' : ks) = k == k' || pertenece k ks


{-Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
su cantidad de ocurrencias.-}
multiSetToList :: Eq a => MultiSet a -> [(a, Int)]
multiSetToList (MS map) = claveValor (keys map) map

claveValor :: Eq a =>[a] ->  Map a Int -> [(a, Int)]
claveValor [] _ = []
claveValor (k : ks) m = (k, valor'(lookupM k m)) : claveValor ks m

valor' :: Maybe Int -> Int
valor' (Just v) = v
valor' Nothing = 0
