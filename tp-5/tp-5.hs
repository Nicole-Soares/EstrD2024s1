import Set
import Queue
import Stack


-- Modo Usuario ( NO SE PUEDE HACER PM EN LA PARTE DE USUARIO CON TIPOS ABSTRACTOS)
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

------------------------------Set
set1 = addS 8 (addS 5 (addS 4 emptyS))
tree1 = NodeT set1 EmptyT EmptyT

--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _ = []
losQuePertenecen (x : xs) set = if belongs x set
                                    then x :  losQuePertenecen xs set
                                    else losQuePertenecen xs set
        

--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (listToSet xs)

listToSet :: Eq a => [a] -> Set a 
listToSet [] = emptyS
listToSet (x:xs)  = addS x (listToSet xs)


----Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT set izq der) = unionS (unionS set (unirTodos izq)) (unirTodos der)

--------------------------------Queue

queue1 = enqueue 10 (enqueue 20 emptyQ)
queue2 = enqueue 30 emptyQ
--Cuenta la cantidad de elementos de la cola.
lengthQ :: Queue a -> Int
lengthQ queue = if isEmptyQ queue
                 then 0
                 else 1 + lengthQ (dequeue queue)


{-Dada una cola devuelve la lista con los mismos elementos,
donde el orden de la lista es el de la cola.
Nota: chequear que los elementos queden en el orden correcto.-}
queueToList :: Queue a -> [a]
queueToList queue = if isEmptyQ queue
                     then []
                     else firstQ queue : queueToList (dequeue queue)


--Inserta todos los elementos de la segunda cola en la primera.
unionQ :: Queue a -> Queue a -> Queue a
unionQ queue1 queue2 = if isEmptyQ queue2
                        then queue1
                        else enqueue (firstQ queue2) (unionQ queue1 (dequeue queue2))


--------------------------------Stack

stack1 = push 20 ((push 10(push 5 emptySt))) -- [5 , 10, 20]

--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a --[5, 10, 20]
apilar [] = emptySt
apilar lista = apilar' (reverse lista) --[20, 10, 5] -- lo doy vuelta porque sino el primer elemento de la lista pero lo pone como primer elemento del stack (que vendria a ser el ultimo de la lista)

apilar' :: [a] -> Stack a
apilar' [] = emptySt
apilar' (x : xs) =  push x (apilar' xs) -- en este caso no me importa cual es el ultimo en recibir, ya que me piden que la lista se mantenga como me viene
    


                                    

--Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a] 
desapilar stack = if isEmptyS stack
                    then []
                    else reverse (top stack : desapilar (pop stack))
                                    -- 10    : 5

{-Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).-}
insertarEnPos :: Int -> a -> Stack a -> Stack a
{-
    INV.REP.: la posición dada tiene que ser una válida en el stack dado
-}
insertarEnPos 0 x s = push x s
insertarEnPos n x s =  push (top s)  (insertarEnPos (n - 1) x (pop s))

{-insertarEnPos' :: Int -> a -> [a] -> Stack a
insertarEnPos' _ _ [] = emptySt
insertarEnPos' 0 x ys = push x (apilar ys)
insertarEnPos' n x (y : ys) =  push y (insertarEnPos' (n - 1) x ys)-}