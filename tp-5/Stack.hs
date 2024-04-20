module Stack --LIFO (last in,first out). Pila
    (Stack, emptySt, isEmptyS, push, top, pop)
where


data Stack a = S [a]                     Int -- se creo esto para que lenS sea constante, para no tener que ir recorriendo toda la lista y contando siempre, ya que si la lista es muy larga eso no es eficiente
     deriving Show          
        --lista de elementos en la pila, cantidad de elementos en la pila
{-
    INV.REP.: = en S lista n
                * n es la longitud de la lista
                
-}

stack1 = push 10(push 5 emptySt) -- [5 , 10, 20]
--Implementación-----------------------------------------------------------

--Crea una pila vacía.
--O(1)
emptySt :: Stack a 
emptySt = S [] 0

--Dada una pila indica si está vacía.
--O(1)
isEmptyS :: Stack a -> Bool 
isEmptyS (S lista n) = n == 0 -- || null lista ??

--Dados un elemento y una pila, agrega el elemento a la pila. // el ultimo en recibir sale primero
--O(1)
push :: a -> Stack a -> Stack a 
push x (S lista n) = S (x : lista) (n + 1) --   S (agregarAlFinal lista x) (n + 1) | para que sea constante

--Dada un pila devuelve el elemento del tope de la pila.
--O(n)
top :: Stack a -> a 
top(S lista _) = head (reverse lista) -- ultimo en entrar, primero en salir

 --Dada una pila devuelve la pila sin el primer elemento

pop :: Stack a -> Stack a
pop (S lista n) = S (reverse (tail (reverse lista))) (n - 1) -- doy vuelta la lista para sacar el primer elemento (el ultimo en entrar) y que me retorne el resto y lo vuelvo a poner en su posicion

--Dada la cantidad de elementos en la pila.
--O(1)
lenS :: Stack a -> Int 
lenS (S lista n) = n




----Practica 2
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal (x : xs) e = x : agregarAlFinal xs e --saca al primer elemento de la lista y lo agrega despues en la recursion

