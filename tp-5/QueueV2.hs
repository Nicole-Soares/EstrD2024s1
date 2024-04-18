{-
    2. Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare
        la eficiencia entre ambas implementaciones.

-}


module QueueV2
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

data Queue a = Q [a] deriving Show
                -- lista de a

{--}
emptyQ :: Queue a --Crea una cola vacía. | Costo = Constante
isEmptyQ :: Queue a -> Bool --Dada una cola indica si la cola está vacía. | Costo = Constante
enqueue :: a -> Queue a -> Queue a --Dados un elemento y una cola, agrega ese elemento a la cola. | Costo = Constante
firstQ :: Queue a -> a --Dada una cola devuelve el primer elemento de la cola. / agrega adelante | Parcial | Costo = Lineal
dequeue :: Queue a -> Queue a --Dada una cola la devuelve sin su primer elemento. | Parcial | Costo = Lineal

--Implementación de las funciones dadas
emptyQ = Q []
isEmptyQ (Q xs) = null xs
enqueue x (Q ys) = Q (x:ys) -- agrega por delante
firstQ (Q xs) = head (reverse xs) -- doy vuelta la lista y quito el "ultimo" de la lista en su formato original
dequeue (Q xs) = Q (reverse (tail (reverse xs))) -- doy vuelta, le saco el primer elemento (osea el ultimo en su formato original) uso tail para retornar el resto y lo doy vuelta de nuevo para que quede en su formato original


