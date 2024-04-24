--Queue con dos listas

module QueueV3
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where



data Queue a = Q [a]                        [a]
        -- lista de a fs (front stack) y  bs (back stack)

{-
    INV.REP.: Q fs bs
            * si fs es null, entonces la cola esta vacía.
-}
emptyQ :: Queue a --Crea una cola vacía. | Costo: Constante
isEmptyQ :: Queue a -> Bool --Dada una cola indica si la cola está vacía. | Costo: Constante
enqueue :: a -> Queue a -> Queue a --Dados un elemento y una cola, agrega ese elemento a la cola.| Costo: Constante
firstQ :: Queue a -> a --Dada una cola devuelve el primer elemento de la cola. PARCIAL preguntar?? | Costo: Constante
dequeue :: Queue a -> Queue a --Dada una cola la devuelve sin su primer elemento. PARCIAL | Costo: Constante amortizado

--Implementación de las funciones dadas
emptyQ = Q [] [] 
isEmptyQ (Q fs bs) = null fs 
enqueue x (Q fs bs) = if null fs 
                        then Q (x : fs)  bs
                        else Q fs (x : bs) -- bs esta dado vuelta, por eso despues hago reverse preguntar??
firstQ (Q fs bs) = head fs 
dequeue (Q fs bs) = if null fs 
                    then Q (tail (reverse bs)) [] -- constante amortizada porque es constante pero muy pocas veces puede que no sea (cuando fs este vacía y tengamos que pasarle lo de bs ahi es un caso no constante)
                    else Q (tail fs) bs

--PREGUNTAR SI ESTA BIEN