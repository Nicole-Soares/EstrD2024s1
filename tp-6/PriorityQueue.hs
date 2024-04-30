module PriorityQueue -- Cola FIFO (first in, first out, ordenado de menor a mayor)
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)

where
data PriorityQueue a = PQ [a] deriving Show
                        --lista de elementos de menor a mayor (el menor tiene mas prioridad)

{-
    INV.REP.:
-}

priory1 = insertPQ 1 priory2
priory2 = insertPQ 4 emptyPQ
--implementacion

--Propósito: devuelve una priority queue vacía
--O(1) 
emptyPQ :: PriorityQueue a
emptyPQ = PQ []

--Propósito: indica si la priority queue está vacía.
--O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs

--Propósito: inserta un elemento en la priority queue
--O(n por el costo de agregarElementosEnOrden)
--O(n)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ e (PQ xs) = PQ (agregarElementoEnOrden e xs)

--devuelve una lista con el elemento agregado de menor a mayor
--O( n es el largo de la lista dada porque hace recursion sobre si misma y por cada elemento tiene un constante (la comparacion))
--O(n)
agregarElementoEnOrden :: Ord a => a -> [a] -> [a]
agregarElementoEnOrden e [] = [e]
agregarElementoEnOrden e (x : xs) = if e < x 
                                     then e : (x : xs)
                                     else x : agregarElementoEnOrden e xs

--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
--O(1)
findMinPQ :: Ord a => PriorityQueue a -> a  
findMinPQ (PQ xs) = head xs                                 

--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
--O(1)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ xs) = PQ (tail xs)