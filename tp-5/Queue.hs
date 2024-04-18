module Queue -- Cola FIFO (first in, first out)
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where


data Queue a = Q [a] deriving Show
                -- lista de a

{--}
emptyQ :: Queue a --Crea una cola vacía.
isEmptyQ :: Queue a -> Bool --Dada una cola indica si la cola está vacía.
enqueue :: a -> Queue a -> Queue a --Dados un elemento y una cola, agrega ese elemento a la cola. / agrega por detras
firstQ :: Queue a -> a --Dada una cola devuelve el primer elemento de la cola.
dequeue :: Queue a -> Queue a --Dada una cola la devuelve sin su primer elemento.

--Implementación de las funciones dadas
emptyQ = Q []
isEmptyQ (Q xs) = null xs
enqueue x (Q ys) = Q (agregarAlFinal ys x) -- Q (ys ++ [x])
firstQ (Q xs) = head xs
dequeue (Q xs) = Q (tail xs)

----Practica 2
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal (x : xs) e = x : agregarAlFinal xs e


