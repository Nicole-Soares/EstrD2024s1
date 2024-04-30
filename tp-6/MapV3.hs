{-module Map --dos listas, una con clave y otra con los valores, en orden la primera clave con el primer valor
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where


data Map k v = M [k] [v]deriving Show
            --lista de claves, lista de valores

    {- INV.REP.: (M ks vs)
        En kvs no hay  claves repetidas

    -}

--implementacion

--Propósito: devuelve un map vacío 
--O(1)
emptyM :: Map k v
emptyM = M []
   
--Propósito: agrega una asociación clave-valor al map.
--O(n porque su subtarea agregar es lineal)
--O(n)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M ks vs) = if not pertenece k ks
                        then agregar k
    
    M (agregar k ks ) agregar (v vs (posicionDeK k ks))

--O(n que es el largo de la lista ya que hace recursion sobre si misma y en el peor de los casos llega hasta vaciar la lista)
--O(n)
agregar :: Eq k => k -> [k] -> [k] 
agregar k (k':ks) = if not pertenece k
agregar k v [] = [k] --si no encontro el mismo k entonces lo agrega
agregar k v (k' : ks) = if k == k'
                                 then  ks -- si encontro el mismo k le cambia su valor
                                 else  k': agregar k ks





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
                             then  kvs
                             else (k', v) : borrar k kvs


--Propósito: devuelve las claves del map.
keys :: Map k v -> [k]
keys (M kvs) = claves kvs

claves :: [(k, v)] -> [k]
claves [] = []
claves ((k , v) : kvs) = k : claves kvs
-}