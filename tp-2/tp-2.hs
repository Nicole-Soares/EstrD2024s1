-- 1 RECURSIÓN SOBRE LISTAS

--1.1

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n : ns) = n +  sumatoria ns

--1.2

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

--1.3

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x : xs) = (x + 1) : sucesores xs

--1.4

conjuncion :: [Bool] -> Bool
conjuncion [] = False -- si ya me viene una lista vacia es false
conjuncion (b : [] ) = b  -- si solo queda un elemento y el resto es una lista vacia, devuelvo el valor de ese elemento
conjuncion (b : bs) = if not b
                        then False
                        else conjuncion bs
    
{-
conjuncion :: [Bool] -> Bool
conjuncion [] = False -- si ya me viene una lista vacia es false
conjuncion (b : [] ) = b  -- si solo queda un elemento y el resto es una lista vacia, devuelvo el valor de ese elemento
conjuncion (False : xs ) = False
conjuncion (b : bs) = b && conjuncion bs
-}  

-- 1.5 

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b : []) = b
disyuncion (b : bs) = if b 
                        then True -- b
                        else disyuncion bs

--1.6

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (l : ls) = l ++ aplanar ls

--1.7

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
--pertenece x (y : []) = x == y 
pertenece x (y : ys) = if x == y
                        then True
                        else pertenece x ys
    
    
--1.8

apariciones :: Eq a => a -> [a] -> Int
--PRECOND: el elemento pasado por parámetro debera ser del mismo tipo que la lista pasada por parámetro
apariciones _ [] = 0
apariciones e (x : xs) = if e == x
                            then 1 + apariciones e xs
                            else apariciones e xs   


--1.9

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA n (x : xs) = if n > x 
                        then x : losMenoresA n xs
                        else losMenoresA n xs


--1.10  

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x : xs) = if longitud x > n
                                    then x : lasDeLongitudMayorA n xs   
                                    else lasDeLongitudMayorA n xs

--1.11

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal xs y = xs ++ y : [] 

--1.12

agregar :: [a] -> [a] -> [a]
agregar xs ys = xs ++ ys

--1.13

reversa :: [a] -> [a]
reversa xs = reverse xs

--1.14

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos ns [] = ns
zipMaximos [] ns = ns
zipMaximos (x : xs) (y : ys) = maximo x y : zipMaximos xs ys

maximo :: Int -> Int -> Int
maximo n1 n2 = if n1 > n2           
                then n1
                else n2

--1.15

elMinimo :: Ord a => [a] -> a
elMinimo xs = minimum xs


--2 RECURSIÓN SOBRE NÚMEROS

--2.1

factorial :: Int -> Int
--PRECOND: n no puede ser un número negativo
factorial 0 = 1
factorial n = n * factorial(n - 1)


--2.2

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

--2.3

repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e : repetir (n-1) e

--2.4

losPrimeros :: Int -> [a] -> [a]
losPrimeros _ [] = []
losPrimeros 0 _ = []
losPrimeros n (x : xs) = x : losPrimeros (n - 1) xs  

--2.5

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ [] = []
sinLosPrimeros 0 xs = xs
sinLosPrimeros n (x : xs) = sinLosPrimeros (n - 1) xs

--3 REGISTROS
--3.1
data Persona = P  String  Int deriving Show 
                --Nombre Edad
           

edad :: Persona -> Int
edad (P n e) = e     

listaDeEdadesDePersonas :: [Persona] -> [Int]
listaDeEdadesDePersonas [] = []
listaDeEdadesDePersonas (p : ps) = edad p :  listaDeEdadesDePersonas ps

persona1 = P "Nicole" 25
persona2 = P "Nacho"  33
persona3 = P "Marta"  53

--3.1.a

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (p : ps) = if edad p > n
                        then p : mayoresA n ps
                        else mayoresA n ps

--3.1.b                       

promedioEdad :: [Persona] -> Int
--PRECOND: la lista tiene al menos una persona
promedioEdad ps =  div (sumatoria (listaDeEdadesDePersonas ps)) (longitud ps) 

--3.1.c

elMasViejo :: [Persona] -> Persona
--PRECOND:  la lista al menos posee una persona
elMasViejo (p : []) = p
elMasViejo (p : ps) = if edad p > edad (elMasViejo ps)
                        then p
                        else elMasViejo ps
    

--3.2

data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int deriving Show
data Entrenador = ConsEntrenador String [Pokemon] deriving Show

entrenador1 = ConsEntrenador "Nicole" [pokemon1, pokemon2, pokemon3]
entrenador2 = ConsEntrenador "Nacho" [pokemon1, pokemon2]

pokemon1 = ConsPokemon Fuego 30
pokemon2 =  ConsPokemon Agua 5
pokemon3 = ConsPokemon Planta 10

--3.2.a

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador n ps) = longitud ps

--3.2.b

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ ps) = cantPokemonesDelTipo ps t  

cantPokemonesDelTipo :: [Pokemon] -> TipoDePokemon -> Int
cantPokemonesDelTipo [] _ = 0
cantPokemonesDelTipo (p : ps) t = if mismoTipoDelPokemon (tipoDelPokemon p) t
                                    then 1 + cantPokemonesDelTipo ps t
                                    else cantPokemonesDelTipo ps t

tipoDelPokemon :: Pokemon -> TipoDePokemon
tipoDelPokemon (ConsPokemon t _ )  = t

mismoTipoDelPokemon :: TipoDePokemon -> TipoDePokemon -> Bool
mismoTipoDelPokemon Agua Agua = True
mismoTipoDelPokemon Fuego Fuego = True
mismoTipoDelPokemon Planta Planta = True
mismoTipoDelPokemon _ _ = False

--3.2.c

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = cantPokemonesGanadores (pokemonesDelTipo (pokemonesDelEntrenador e1) t ) (pokemonesDelEntrenador e2)


pokemonesDelEntrenador :: Entrenador -> [Pokemon]
--te retorna la lista de pokemones del entrenador pasado como argumento
pokemonesDelEntrenador (ConsEntrenador _ ps) = ps

pokemonesDelTipo :: [Pokemon] -> TipoDePokemon -> [Pokemon]
-- te retorna una lista de los pokemones que son del mismo tipo pasado como argumento
pokemonesDelTipo [] _ = []
pokemonesDelTipo (p : ps) t = if mismoTipoDelPokemon (tipoDelPokemon p) t
                                then p : pokemonesDelTipo ps t
                                else pokemonesDelTipo ps t


cantPokemonesGanadores :: [Pokemon] -> [Pokemon] -> Int
--te retorna la cantidad de pokemones de la primera lista pasada que le ganan a todos los pokemones de la segunda lista pasada
cantPokemonesGanadores [] _ = 0
cantPokemonesGanadores _ [] = 0
cantPokemonesGanadores (p1 : p1s) p2s = if pokemonLeGanaAOtrosPokemones p1 p2s
                                                then 1 + cantPokemonesGanadores p1s p2s
                                                else  cantPokemonesGanadores p1s p2s


pokemonLeGanaAOtrosPokemones :: Pokemon -> [Pokemon] -> Bool
--
pokemonLeGanaAOtrosPokemones _ [] = True
pokemonLeGanaAOtrosPokemones p1 (p2 : p2s) = tipoGanador (tipoDelPokemon p1) (tipoDelPokemon p2) && pokemonLeGanaAOtrosPokemones p1 p2s

{-pokemonLeGanaAOtrosPokemones p1 (p2 : p2s) = if tipoGanador (tipoDelPokemon p1) (tipoDelPokemon p2)  -- hecho con if no se cual es mejor
                                                then pokemonLeGanaAOtrosPokemones p1 p2s
                                                else False-}

tipoGanador :: TipoDePokemon -> TipoDePokemon -> Bool
tipoGanador Agua Fuego = True
tipoGanador Fuego Planta = True
tipoGanador Planta Agua = True
tipoGanador _ _ = False

--3.2.d

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ ps) = pokemonDeCadaTipo ps 

pokemonDeCadaTipo :: [Pokemon]  -> Bool
pokemonDeCadaTipo []  = False
pokemonDeCadaTipo  ps = pokemonDeTipo ps Fuego && pokemonDeTipo ps Agua && pokemonDeTipo ps Planta


pokemonDeTipo :: [Pokemon] -> TipoDePokemon -> Bool
pokemonDeTipo [] _= False
pokemonDeTipo (p : ps) t =  mismoTipoDelPokemon (tipoDelPokemon p) t || pokemonDeTipo ps t

--3.3

data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving (Show, Eq)
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

empresa1 = ConsEmpresa [rol2, rol2, rol3]

rol1= Developer Senior proyecto1 -- puede haber varias personas trabajando en el mismo proyecto
rol2 = Developer Senior proyecto2
rol3 = Developer Junior proyecto1

proyecto1= ConsProyecto "Netflix"
proyecto2 = ConsProyecto "Spotify"
proyecto3 = ConsProyecto "Youtube"

--3.3.a

proyectos :: Empresa -> [Proyecto]
-- recibe una empresa y retorna una lista de los proyectos de esa empresa sin repetir
proyectos (ConsEmpresa rs) = proyectosDeLosRoles rs

proyectosDeLosRoles :: [Rol] -> [Proyecto]
-- recibe una lista de roles y retorna una lista de todos los proyectos de esos roles sin repetir
proyectosDeLosRoles [] = []
proyectosDeLosRoles  (r : rs) =  if not (contiene (proyectosDeLosRoles rs)  (proyectoDelRolActual r)) 
                                    then proyectoDelRolActual r : proyectosDeLosRoles rs
                                    else proyectosDeLosRoles rs
    
proyectoDelRolActual :: Rol -> Proyecto
--recibe un rol y devuelve el proyecto de ese rol
proyectoDelRolActual  (Developer _ p) = p
proyectoDelRolActual  (Management _ p) = p 

contiene :: [Proyecto] -> Proyecto -> Bool
--recibe una lista de proyectos, un proyecto y retorna si ese proyecto esta en la lista dada
contiene [] _ = False
contiene (p1 : p2s) p2 = nombreDelProyecto p1 == nombreDelProyecto p2 || contiene p2s p1

nombreDelProyecto :: Proyecto -> String
nombreDelProyecto (ConsProyecto n) = n

--3.3.b

--Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen además a los proyectos dados por parámetro
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps = length (proyectosEnComun(proyectosSenior rs) ps)

proyectosSenior :: [Rol] -> [Proyecto]
proyectosSenior [] = []
proyectosSenior (r : rs) = proyectoSeniorDelRolActual r ++  proyectosSenior rs 

proyectoSeniorDelRolActual :: Rol -> [Proyecto]
proyectoSeniorDelRolActual (Developer Senior p) = [p]
proyectoSeniorDelRolActual (Management Senior p) = [p]
proyectoSeniorDelRolActual _ = []

-- devuelve una lista de proyectos que se encuentran en ambas listas
proyectosEnComun :: [Proyecto] -> [Proyecto] -> [Proyecto]
proyectosEnComun [] _ = []
proyectosEnComun _ [] = []
proyectosEnComun (p : ps1) ps2 = if listaProyectosContiene ps2 p
                                    then p : proyectosEnComun ps1 ps2
                                    else proyectosEnComun ps1 ps2

listaProyectosContiene :: [Proyecto] -> Proyecto -> Bool
listaProyectosContiene [] _ = False
listaProyectosContiene (p1 : p1s) p2 = mismoProyecto p1 p2 || listaProyectosContiene p1s p2

mismoProyecto :: Proyecto -> Proyecto -> Bool
mismoProyecto (ConsProyecto n1) (ConsProyecto n2) = n1 == n2

--3.3.c

--traigo a todos los empleados de todos los roles de la empresa dada
-- veo cuando proyecto de cada rol coincide con la lista de proyectos dada
-- cuento todos los roles

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) = cantDeEmpleadosQueTrabajanEn rs ps

cantDeEmpleadosQueTrabajanEn :: [Rol] -> [Proyecto] -> Int
-- Describe la cantidad de empleados en total que trabajan en la lista de proyectos dada
cantDeEmpleadosQueTrabajanEn [] _ = 0
cantDeEmpleadosQueTrabajanEn _ [] = 0
cantDeEmpleadosQueTrabajanEn (r : rs) ps =    if contiene ps (proyectoDelRolActual r) --si el proyecto del rol se encuentra en la lista de proyectos, entonces ese empleado trabaja en algun proyecto de los pasados por parámetro
                                                then 1 + cantDeEmpleadosQueTrabajanEn rs ps
                                                else cantDeEmpleadosQueTrabajanEn rs ps


--3.3.d

--tener una lista de todos los proyectos de la empresa dada, abrirla y creo una subtarea el cual sumo la cantidad de empleados del proyecto pasado y asi con el resto, una recursion
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
--Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su cantidad de personas involucradas.
asignadosPorProyecto e = listaDeTuplasDeEmpleadosYSuProyecto (proyectos e) (rolesDeLaEmpresa e)

listaDeTuplasDeEmpleadosYSuProyecto  :: [Proyecto] -> [Rol]  -> [(Proyecto, Int)] --recorre proyectos para saber la cantidad de empleados de cada uno
listaDeTuplasDeEmpleadosYSuProyecto [] _        = []
listaDeTuplasDeEmpleadosYSuProyecto _ []        = []
listaDeTuplasDeEmpleadosYSuProyecto (p : ps) rs  =  (p, cantEmpleadosPorProyecto p rs)  : listaDeTuplasDeEmpleadosYSuProyecto ps rs


cantEmpleadosPorProyecto :: Proyecto -> [Rol] -> Int -- recorre roles 
cantEmpleadosPorProyecto _ []       = 0
cantEmpleadosPorProyecto p (r : rs) =   if mismoProyecto p (proyectoDelRol r)         
                                            then 1 + cantEmpleadosPorProyecto p rs
                                            else cantEmpleadosPorProyecto p rs
    

proyectoDelRol :: Rol -> Proyecto
proyectoDelRol (Developer _ p) = p
proyectoDelRol (Management _ p) = p

rolesDeLaEmpresa :: Empresa -> [Rol]
rolesDeLaEmpresa (ConsEmpresa rs) = rs