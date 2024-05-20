module Empresa
    (Empresa, consEmpresa, empleadosDelSector)
where

import Map
import Set

type SectorId = Int
type CUIL = Int


data Empleado = E String deriving (Show, Eq)
data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)
{-

INV.REP: en (ConsE mapSector mapCuil)
        
-}


{-mapSectores = assocM 1 ["pedro","juan"] (assocM 2 ["pedro","Rober"] emptyM)

mapCuilEmpleados = assocM 1 ["pedro","juan"] (assocM 2 ["pedro","Rober"] emptyM)

empresa = ConsE (mapSectores) (mapCuilEmpleados)-}

{-Propósito: construye una empresa vacía.
Costo: O(1)-}
consEmpresa :: Empresa
consEmpresa = ConsE emptyM emptyM

{-
Propósito: devuelve el empleado con dicho CUIL.
Precondición: el CUIL es de un empleado de la empresa.
Costo: O(log E)
-}
buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL c (ConsE _ mapCuil) = buscarPorCUILM c mapCuil

buscarPorCUILM :: CUIL -> Map CUIL Empleado -> Empleado
--Precondición: el CUIL es de un empleado de la empresa.
buscarPorCUILM c map = lookupM c map

{-
Propósito: indica los empleados que trabajan en un sector dado.
Costo: O(log S + E)
-}
empleadosDelSector ::  SectorId -> Empresa -> [Empleado]
empleadosDelSector sId (ConsE map _) = empleadosDelSectorM sId map

empleadosDelSectorM :: SectorId -> Map SectorId (Set Empleado) -> [Empleado]
empleadosDelSectorM sId map = lista(lookupM sId map)

lista :: Maybe (Set Empleado) -> [Empleado]
lista Nothing = []
lista (Just e) = setToList e

{-
Propósito: indica todos los CUIL de empleados de la empresa.
Costo: O(E)
-}
todosLosCUIL :: Empresa -> [CUIL]
todosLosCUIL (ConsE _ map) = todosLosCUILM map

todosLosCUILM :: Map CUIL Empleado -> [CUIL]
todosLosCUILM m =  keys m


{-
Propósito: indica todos los sectores de la empresa.
Costo: O(S)-}
todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE map _) = todosLosSectoresM map

todosLosSectoresM :: Map SectorId (Set Empleado) -> [SectorId]
todosLosSectoresM m = keys m

{-
Propósito: agrega un sector a la empresa, inicialmente sin empleados.
Costo: O(log S)
-}
agregarSector :: SectorId -> Empresa -> Empresa
agregarSector sId (ConsE m1 m2) = ConsE (assocM sId emptyS m1) m2


{-
Propósito: agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el
CUIL dado.
Costo: calcular.
-}
--O(log k # por la subatarea de agregarEmpleadoMapCuil
-- +
-- s + log k + n + log k # por la subatarea de agregarEmpleadoMap )
--O( log k + s + log k + n + log k)
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado sectoresId cuil (ConsE mapSector mapCuil) = ConsE (agregarEmpleadoMap sectoresId cuil mapSector) (agregarEmpleadoMapCuil cuil mapCuil)
--                                                                   O(s + log k + n + log k)                     O (log k)

--O(s# es la longitud de [SectorId] la cual hace recursion sobre la misma
-- +
-- log K # por la subatarea de lookupM, la cual trae el valor de la clave en el mapa dado
-- + 
-- 1# por la subtarea de consEmpleado
-- +
-- n # por la subatarea de addS la cual agrega un elemento a un set
-- +
-- log K # por la subatarea de assocM la cual dandole una clave, valor y map, lo agrega en este último)
--O(s + log k + 1 + n + log k)     
--O(s + log k + n + log k)                                                                                                         
agregarEmpleadoMap :: [SectorId] -> CUIL -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarEmpleadoMap [] _ map = map
agregarEmpleadoMap (sId : sIds) cuil map = assocM sId (addS (consEmpleado cuil) (lookupM sId map)) (agregarEmpleadoMap sIds cuil map)

-- O ( 1 # por la subatara de consEmpleado
-- +
-- (log K) # por la subatarea de assocM)
-- O( 1 + log k)
--O (log k)
agregarEmpleadoMapCuil :: CUIL -> Map CUIL Empleado -> Map CUIL Empleado
agregarEmpleadoMapCuil cuil map = assocM cuil (consEmpleado cuil) map

{-
Propósito: agrega un sector al empleado con dicho CUIL.
Costo: calcular.
-}

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa --Precond: el cuil existe en la empresa
agregarASector sectorId cuil (ConsE mapSector mapCuil) = ConsE (agregarASectorMap sectorId mapSector cuil mapCuil) (agregarASectorMapCuil mapCuil sectorId cuil)


agregarASectorMap :: SectorId -> Map SectorId (Set Empleado) -> CUIL -> Map CUIL Empleado -> Map SectorId (Set Empleado)
agregarASectorMap sectorId mapSector cuil mapCuil = assocM sectorId (addS (lookupM cuil mapCuil) (lookupM sectorId mapSector)) mapSector

agregarASectorMapCuil :: Map CUIL Empleado -> SectorId -> CUIL -> Map CUIL Empleado
agregarASectorMapCuil mapCuil sectorId cuil = assocM cuil (incorporarSector sectorId (lookupM cuil mapCuil)) mapCuil

{-
Propósito: elimina al empleado que posee dicho CUIL.
Costo: calcular.
-}
borrarEmpleado :: CUIL -> Empresa -> Empresa
borrarEmpleado cuil (ConsE mapSectorId mapCuil) = ConsE (borrarEmpleadoMapSectorId mapSectorId (lookupM cuil mapCuil)) (borrarEmpleadoMapCuil cuil mapCuil)

borrarEmpleadoMapCuil :: CUIL -> Map CUIL Empleado -> Map CUIL Empleado
borrarEmpleadoMapCuil cuil map = deleteM cuil map

borrarEmpleadoMapSectorId :: Map SectorId (Set Empleado) -> Empleado -> Map SectorId (Set Empleado)
borrarEmpleadoMapSectorId  mapSector empleado = borrarEmpleadoMapSectorId' (keys mapSector) (sectores empleado) empleado mapSector


borrarEmpleadoMapSectorId' :: [SectorId] -> [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
borrarEmpleadoMapSectorId' [] _ _ map = map
borrarEmpleadoMapSectorId' (id : ids) ids' empleado map = if pertenece id ids'
                                                            then assocM id (removeS empleado (lookupM id map)) map
                                                            else borrarEmpleadoMapSectorId' ids ids' empleado map