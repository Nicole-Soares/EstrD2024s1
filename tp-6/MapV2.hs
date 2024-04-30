module MapV2 --claves repetidas
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where


data Map k v = M [(k, v)] deriving Show
                -- lista de tuplas clave (k) y valor (v)

    {- INV.REP.: (M kvs)
       

    -}

--implementacion

--Propósito: devuelve un map vacío 
--O(1)
emptyM :: Map k v
emptyM = M []
   
--Propósito: agrega una asociación clave-valor al map.

--O(1)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M kvs) = M((k, v) : kvs) -- lo agrego sin importar si ya existe en la lista
   

{---O(n que es el largo de la lista ya que hace recursion sobre si misma y en el peor de los casos llega hasta vaciar la lista)
--O(n)
agregar :: Eq k => k -> v -> [(k, v)] -> [(k, v)] 
agregar k v [] = [(k, v)] --si no encontro el mismo k entonces lo agrega
agregar k v ((k', v') : kvs) = if k == k'
                                 then (k', v) : kvs -- si encontro el mismo k le cambia su valor
                                 else  (k', v') : agregar k v kvs

-}



--Propósito: encuentra un valor dado una clave.
--O(n porque el valor de su subtarea valor es lineal)
--O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M kvs) = valor k kvs

--O(n es la longitud de la lista ya que hace recursion sobre si misma)
--O(n)
valor :: Eq k => k -> [(k, v)] -> Maybe v
valor _ [] = Nothing
valor k (( k', v) : kvs) = if k == k'
                            then Just v
                            else valor k kvs

--Propósito: borra una asociación dada una clave.
--O(n porque el valor de su subtarea valor es borrar)
--O(n)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M kvs) = M (borrar k kvs )

--O(n es la longitud de la lista ya que hace recursion sobre si misma)
--O(n)
borrar ::Eq k =>  k -> [( k, v)] -> [(k , v)]
borrar _ [] = []
borrar k ((k', v) : kvs) = if k == k'
                             then  borrar k kvs -- porque puede aparecer mas de una vez
                             else (k', v) : borrar k kvs


--Propósito: devuelve las claves del map.
keys :: Map k v -> [k]
keys (M kvs) = claves kvs

claves :: [(k, v)] -> [k]
claves [] = []
claves ((k , v) : kvs) = k : claves kvs
