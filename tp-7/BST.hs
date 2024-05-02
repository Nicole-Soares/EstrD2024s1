

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

{-
Propósito: dado un BST dice si el elemento pertenece o no al árbol.
Costo: O(log N)
-}
belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST x EmptyT = False
belongsBST x (NodeT y ti td) = if (x==y)
                                then True
                                else if x < y
                                    then belongsBST x ti
                                    else belongsBST x td 


{-
Propósito: dado un BST inserta un elemento en el árbol.
Costo: O(log N)
-}
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x EmptyT = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if x == y
                                then NodeT x ti td
                                else if x < y 
                                    then NodeT y (insertBST x ti) td
                                    else NodeT y ti (insertBST x td)

{-
Propósito: dado un BST borra un elemento en el árbol.
Costo: O(log N)
-}
deleteBST :: Ord a => a -> Tree a -> Tree a
--PRECOND: el arbol es BST
deleteBST x EmptyT = EmptyT
deleteBST x (NodeT y ti td) = if x == y
                                then rearmar ti td
                                else if x < y
                                    then NodeT y (deleteBST x ti) td
                                    else NodeT y ti (deleteBST x td)


rearmar :: Ord a =>  Tree a -> Tree a -> Tree a
--PRECOND: los dos arboles son BST
rearmar  EmptyT td = td
rearmar  ti td = let (m , ti') = splitMaxBST ti
                    in NodeT m ti' td

splitMaxBST :: Tree a -> (a , Tree a)
--PRECOND: no puede ser un arbol vacío y tiene que ser un arbol BST
splitMaxBST (NodeT x ti EmptyT) = (x, ti) -- si del lado derecho ya no hay nada, es el actual el mayor
splitMaxBST (NodeT x ti td) = let (m, td') = splitMaxBST td
                                in (m , NodeT x ti td')


{-
Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
Costo: O(log N)
-}
splitMinBST :: Ord a => Tree a -> (a, Tree a)
--precond: el tree es BST y no puede ser vacio
splitMinBST (NodeT x EmptyT td) = (x, td)
splitMinBST(NodeT x ti td) = let (m, ti') = splitMinBST ti
                                in (m, NodeT x ti' td)

tree1 = NodeT 5 ti td
ti = NodeT 2 tii tdd
td= NodeT 6 EmptyT EmptyT
tii = NodeT 1 EmptyT EmptyT
tdd = NodeT 3 EmptyT (NodeT 4 EmptyT EmptyT)

{-
Propósito: indica si el árbol cumple con los invariantes de BST.
Costo: O(N2)
-}
esBST :: Ord a => Tree a -> Bool
esBST EmptyT = True
{-esBST (NodeT x EmptyT td) = esMayor (raiz td) x && esBST td
esBST (NodeT x ti EmptyT) = esMenor (raiz ti) x && esBST ti  -}
esBST (NodeT x ti td) = (esMayor x ti) && (esMenor x td) && esBST ti && esBST td

esMenor :: Ord a => a -> Tree a -> Bool
esMenor x EmptyT = True
esMenor x (NodeT y _ _) = x < y

esMayor :: Ord a => a -> Tree a -> Bool
esMayor x EmptyT = True
esMayor x (NodeT y _ _) = x > y

raiz :: Tree a -> a
--PRECOND: el arbol no esta vacio
raiz (NodeT x _ _) = x

{-
Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
elemento dado.
Costo: O(log N)
-}
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
--PRECOND: tree es BST
elMaximoMenorA _ EmptyT = Nothing
elMaximoMenorA x (NodeT y ti td) = if x == y -- si encontro el elemento
                                    then maximoT ti -- me fijo en su rama izq que son los menores
                                    else if x > y -- si el numero buscado es mayor, sigo buscandolo en la rama derecha
                                        then elMaximoMenorA x td
                                        else elMaximoMenorA x ti 

maximoT :: Tree a -> Maybe a 
maximoT EmptyT = Nothing
maximoT (NodeT x _ EmptyT) = Just x
maximoT (NodeT _ _ td) = maximoT td

{-
Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
elemento dado.
Costo: O(log N)
-}
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
--PRECOND : tree es BST
elMinimoMayorA _ EmptyT = Nothing
elMinimoMayorA x (NodeT y ti td) = if x == y
                                    then minimoT td
                                    else if x > y
                                        then elMinimoMayorA x td
                                        else elMinimoMayorA x ti

minimoT :: Tree a -> Maybe a
minimoT EmptyT = Nothing
minimoT (NodeT x EmptyT _) = Just x --si ya no hay rama izquierda es porque llegue al minimo
minimoT (NodeT _ ti _) =  minimoT ti

{-
Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
Costo: O(N2)
-}
balanceado :: Tree a -> Bool
balanceado EmptyT =  True
balanceado (NodeT _ ti td) = (length(listPerLevel ti)) - (length (listPerLevel td)) <= 1
--balanceado (NodeT _ ti td) = abs((heightT ti) - (heightT td)) <= 1 -- abs valor absoluto O(n)

diferenciaEntre :: Int -> Int -> Int
diferenciaEntre x y = x - y

--------------------Práctica 2
listPerLevel :: Tree a -> [[a]]
--Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel EmptyT = []
listPerLevel (NodeT x n1 n2) = [x] : juntarNiveles (listPerLevel n1) (listPerLevel n2)


juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles []    xss = xss
juntarNiveles xss   []  = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss