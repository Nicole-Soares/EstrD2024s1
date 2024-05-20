import PriorityQueue
import Map
import MultiSet


--MODO USUARIO
--EJERCICIO 2 DE PRIORITY QUEUE

{-heapSort :: Ord a => [a] -> [a]
headSort [] =
headSort (x : xs) = (insertPQ x emptyPQ ) headSort xs -}

--O ( n dado por el costo de lineal
--    *
--    n dado el costo de priorityQueue )
--  O(n*n)
-- O n^2


heapSort :: Ord a => [a] -> [a]
heapSort xs = lista (priorityQueue xs)

priorityQueue  :: Ord a => [a] -> PriorityQueue a
priorityQueue [] = emptyPQ 
priorityQueue (x : xs) = insertPQ x (priorityQueue xs)

lista :: Ord a => PriorityQueue a -> [a]
lista pq = if isEmptyPQ pq
            then []
            else findMinPQ pq : lista (deleteMinPQ pq)

----------------------
--MAP

map1 = assocM 5 "Nicole" map2
map2 = emptyM
map3 = assocM 30 "clave1" map2
mapaVacio = emptyM
mapaInicial = assocM "clave1" 10 (assocM "clave2" 20 mapaVacio)


--1
--Propósito: obtiene los valores asociados a cada clave del map.
--O(n^2 # ya que su subtarea (valoresDeLasClaves) es cuadratico)
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = valoresDeLasClaves (keys m) m

--O(n # es la longitud de la lista [k] que se hace recursion sobre la misma
-- * por cada elemento de la lista [k] se hace otra recursion sobre m
--  m # que es la longitud de (m) que se hace recursion sobre la misma)
--O(n * m) = O (n^2)
valoresDeLasClaves:: Eq k => [k] -> Map k v -> [Maybe v]
valoresDeLasClaves [] _ = []
valoresDeLasClaves (k : ks) m = lookupM k m : valoresDeLasClaves ks m

--2
--Propósito: indica si en el map se encuentran todas las claves dadas
----O(n^2 # ya que su subtarea (todasPertenecen) es cuadratico)
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas ks m = todasPertenecen ks (keys m)

--O(n # es la longitud de la lista [k] que hace recursion sobre la misma
-- * por cada elemento de la lista [k] se hace otra recursion sobre ks
-- m # es la longitud de la lista [ks] que hace recursion sobre la misma)
--O(n*m) = O(n^2) o O(m^2)
todasPertenecen :: Eq k => [k] -> [k] -> Bool
todasPertenecen [] _ = True
todasPertenecen (k : ks) ks' = pertenece k ks' && todasPertenecen ks ks'

-----práctica 2
--en el peor de los casos es lineal (?)
pertenece :: Eq k => k -> [k] -> Bool
pertenece _ [] = False
pertenece k (k' : ks) = k == k' || pertenece k ks

--Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap ((k, v) : kvs) = assocM k v (listToMap kvs)

--Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, Maybe v)]
mapToList m = listaDeClaveValor (keys m) m

listaDeClaveValor :: Eq k => [k] -> Map k v -> [(k , Maybe v)]
listaDeClaveValor [] _ = []
listaDeClaveValor (k : ks) m = (k, (lookupM k m)) : listaDeClaveValor ks (deleteM k m)

--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq ((k, v) : kvs) =   assocM k  (v : todosLosValoresDe k kvs )  (deleteM k (agruparEq kvs))


todosLosValoresDe :: Eq k => k -> [(k, v)] -> [v]
todosLosValoresDe _ [] = []
todosLosValoresDe k ((k', v) : kvs) = if k == k'
                                        then v : todosLosValoresDe k kvs
                                        else todosLosValoresDe k kvs


{- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
cada número asociado con dichas claves.-}
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar ks m = mapaIncrementado ks (keys m) m

mapaIncrementado :: Eq k => [k] -> [k] -> Map k Int -> Map k Int
mapaIncrementado [] _ _ = emptyM
mapaIncrementado (k : ks) ks' m = if pertenece k ks'
                                     then mapaIncrementado ks ks' (sumarUnoAlValorDeLaClave k m)
                                     else mapaIncrementado ks ks' m


{-sumarUnoAlValorDeLaClave ::Eq k => k -> Map k Int -> Map k Int 
sumarUnoAlValorDeLaClave k m = assocM k ((valor(lookupM k m)) + 1) m-}

sumarUnoAlValorDeLaClave :: Eq k => k -> Map k Int -> Map k Int 
sumarUnoAlValorDeLaClave k m = case lookupM k m of
                                    Just v  -> assocM k (v + 1) m
                                    Nothing -> m


{-
Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
una clave del primero existe en el segundo, es reemplazada por la del primero.
-}
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = agregarKeys (keys m1) m1 m2

agregarKeys :: Eq k => [k] -> Map k v -> Map k v -> Map k v
agregarKeys [] _ m2 = m2
agregarKeys (k : ks) m1 m2 = case lookupM k m1 of -- busco su valor en su map
                            Just v ->  assocM k v (agregarKeys ks m1 m2) -- lo agrego al otro map
                            Nothing -> agregarKeys ks m1 m2
    

--EJERCICIOS DE MULTISET

--dado un string cuenta la cantidad de ocurrencias de cada caracter en el string
ocurrencias :: String -> MultiSet Char
ocurrencias [] = emptyMS
ocurrencias (char : chars) = addMS char (ocurrencias chars) 