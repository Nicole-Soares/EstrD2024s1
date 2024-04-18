module Stack --LIFO (last in,first out). Pila
    (Stack, emptySt, isEmptyS, push, top, pop)
where


data Stack a = S [a]                     Int -- se creo esto para que lenS sea constante, para no tener que ir recorriendo toda la lista y contando siempre, ya que si la lista es muy larga eso no es eficiente
     deriving Show          
        --lista de elementos en la pila, cantidad de elementos en la pila
{-
    INV.REP.: = en S lista n
                * si lista es vacía, entonces n es 0
                *n es la cantidad total de elementos de lista
-}

stack1 = push 10(push 5 emptySt) -- [5 , 10, 20]
emptySt :: Stack a --Crea una pila vacía.
isEmptyS :: Stack a -> Bool --Dada una pila indica si está vacía.
push :: a -> Stack a -> Stack a --Dados un elemento y una pila, agrega el elemento a la pila. // el ultimo en recibir sale primero
top :: Stack a -> a --Dada un pila devuelve el elemento del tope de la pila.
pop :: Stack a -> Stack a --Dada una pila devuelve la pila sin el primer elemento
lenS :: Stack a -> Int --Dada la cantidad de elementos en la pila.
                       --Costo: constante.

emptySt = S [] 0
isEmptyS (S lista n) = n == 0 -- || null lista ??
push x (S lista n) = S (agregarAlFinal lista x) (n + 1) --  push x (S lista n) = S (x : lista) (n + 1) 
top(S lista _) = head (reverse lista) -- ultimo en entrar, primero en salir
pop (S lista n) = S (reverse (tail (reverse lista))) (n - 1) -- doy vuelta la lista para sacar el primer elemento (el ultimo en entrar) y que me retorne el resto y lo vuelvo a poner en su posicion
lenS (S lista n) = n

----Practica 2
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal (x : xs) e = x : agregarAlFinal xs e --saca al primer elemento de la lista y lo agrega despues en la recursion

--PREGUNTAR LOS COSTOS