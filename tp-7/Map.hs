module Map 
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where
import Data.Sequence (Seq(Empty))


data Tree k v = EmptyT | NodeT (k, v) (Tree k v) (Tree k v)
data Map k v = M (Tree k v)
 {- INV.REP.: en (M t),
 * t cumple ser un BST -}

---Implementacion
--Costo: O(1).
emptyM :: Map k v
emptyM = M(EmptyT)

--Costo: O(log K).
assocM :: Ord k => k -> v -> Map k v -> Map k v
assocM x y (M t) = M (assocT x y t) -- precond: t es BST

assocT :: Ord k => k -> v -> Tree k v -> Tree k v
--Precond: el tree es BST
assocT x y EmptyT = NodeT (x, y) EmptyT EmptyT
assocT x y (NodeT (k', v') ti td) = if x == k'
                                    then NodeT (x, y) ti td
                                    else if x < k'
                                        then NodeT (k', v') (assocT x y ti) td  
                                        else NodeT (k', v') ti (assocT x y td)  

--Propósito: encuentra un valor dado una clave.
--Costo: O(log K).
--
lookupM :: Ord k => k -> Map k v -> Maybe v
lookupM x (M t) = lookupT x t -- Precond: t es BST

lookupT :: Ord k => k -> Tree k v -> Maybe v
lookupT x EmptyT = Nothing
lookupT x (NodeT (k', v') ti td) = if x == k' 
                                    then Just v'
                                    else if x < k'
                                        then lookupT x ti
                                        else lookupT x td

----Propósito: borra una asociación dada una clave.
--Costo: O(log K).
deleteM :: Ord k => k -> Map k v -> Map k v
deleteM x (M t) = M(deleteT x t) -- precond t es BST

deleteT :: Ord k => k -> Tree k v -> Tree k v
deleteT x EmptyT = EmptyT
deleteT x (NodeT (k', v') ti td) = if x == k'
                                    then rearmar ti td
                                    else if x < k'
                                        then (NodeT (k', v') (deleteT x ti) td)
                                        else (NodeT (k', v') ti (deleteT x td))

rearmar :: Ord k =>  Tree k v -> Tree k v -> Tree k v
--PRECOND: los dos arboles son BST
rearmar  EmptyT td = td
rearmar  ti td = let (m , ti') = splitMaxBST ti
                    in NodeT m ti' td

splitMaxBST :: Tree k v -> ((k, v) , Tree k v)
--PRECOND: no puede ser un arbol vacío y tiene que ser un arbol BST
splitMaxBST (NodeT tupla ti EmptyT) = (tupla, ti) -- si del lado derecho ya no hay nada, es el actual el mayor
splitMaxBST (NodeT tupla ti td) = let (tuplaMax, td') = splitMaxBST td
                                in (tuplaMax , NodeT tupla ti td')


--Costo: O(K).
keys :: Map k v -> [k]
keys (M t) = keysT t

keysT :: Tree k v -> [k]
keysT EmptyT = []
keysT (NodeT (k', v') ti td) = k' : (keysT ti ++ keysT td)


{-data AVL k v = EmptyAVL | NodeAVL Int (k, v) (AVL k v) (AVL k v)
 {- INV.REP.: en NodeAVL h x (k, v) ti td
 * h es la altura del árbol
 * (k, v) es clave valor
 * la diferencia de alturas de ti y td es <= 1
 * ti y tf son AVLs -}


data Map k v = M (AVL k v)
{- (M t)
INV.REP.: t cumple con ser un BST y ALV
-}

---Implementacion
--Costo: O(1).
emptyM :: Map k v
emptyM = M(EmptyAVL)

assocM :: Ord k => k -> v -> Map k v -> Map k v
--Costo: O(log K).
--PRECOND:
assocM x y (M t) = M(assocT x y t) 

assocT:: Ord k => k -> v -> AVL k v -> AVL k v  
assocT x y EmptyAVL = NodeAVL 1 (x, y) EmptyAVL EmptyAVL
assocT x y (NodeAVL int (k, v) ti td) = if x == k
                                         then M(NodeAVL int (x, y) ti td)
                                         else if x < k
                                            then NodeAVL int assocM x y ti
                                            else assocM x y td-}

                                         