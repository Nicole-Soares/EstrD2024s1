-- 2. NÚMEROS ENTEROS
-- Ejercício 2.1 

-- a
sucesor :: Int -> Int
sucesor x = x + 1

--b
sumar :: Int -> Int -> Int
sumar x y = x + y

--c
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = ((div x y), mod x y)

--d
maxDelPar :: (Int,Int) -> Int
maxDelPar (x, y) =
            if x > y
                then x
                else y

-- Ejercício 2.2                
 
numeroDiez = maxDelPar (divisionYResto (sumar (sucesor 3) (sucesor 7)) (sucesor (-5)))
numeroDiez2 = maxDelPar (divisionYResto (sumar (sucesor 10) (sucesor (-10))) (sucesor 0))
numeroDiez3 = maxDelPar (divisionYResto (sumar (sucesor 5) (sucesor 5)) (sucesor (-1)))
numeroDiez4 = maxDelPar (divisionYResto (sumar (sucesor 4) (sucesor 6)) (sucesor (-3)))


--3. TIPOS ENUMERATIVOS
 -- Ejercicio 3.1 

data Dir = Norte | Este | Sur | Oeste
 deriving Show
 --a.
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Oeste = Este
opuesto Este = Oeste

--b.
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales Este Este = True
iguales d d1 = False

--c
siguiente :: Dir -> Dir
--PRECOND : Dir debe ser diferente a Oeste
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste

{-
    Es una función parcial ya que lleva una precondición porque no hay una dirección siguiente a Oeste
-}

-- Ejercicio 3.2

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves |Viernes | Sabado | Domingo
 deriving Show

--a.
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

--b
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM d = False

--c 
{-
    le pongo a cada dia un número, para poder saber el mayor y asi ver si  el dia1 es posterior al dia2
-}
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = numeroDeDia dia1 > numeroDeDia dia2

numeroDeDia :: DiaDeSemana -> Int
numeroDeDia Lunes = 1
numeroDeDia Martes = 2
numeroDeDia Miercoles = 3
numeroDeDia Jueves = 4
numeroDeDia Viernes = 5
numeroDeDia Sabado = 6
numeroDeDia Domingo = 7

--d
estaEnElMedio :: DiaDeSemana -> Bool
{-estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio d = True
-}
estaEnElMedio dia = not (esLunes dia) && not (esDomingo dia)

esLunes :: DiaDeSemana -> Bool
esLunes Lunes = True
esLunes d = False

esDomingo :: DiaDeSemana -> Bool
esDomingo Domingo = True
esDomingo d = False

-- Ejercicio 3.3

--a.
negar :: Bool -> Bool
negar True = False
negar False = True

--b
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

--sin doble pattern matching.
implica2 :: Bool -> Bool -> Bool
implica2 _  True = True
implica2 False _ = True
implica2 _ _     = False


--c
{-yTambien :: Bool -> Bool -> Bool
yTambien bol1 bol2 = bol1 && bol2 -}
yTambien :: Bool -> Bool -> Bool
yTambien  True True = True
yTambien _ _ = False


--sin doble pattern matching.
yTambien2 :: Bool -> Bool -> Bool
yTambien2 True b = b
yTambien2 _ _ = False


--d
oBien :: Bool -> Bool -> Bool
oBien False False = False
oBien _ _ = True


--sin doble pattern matching.
oBien2 :: Bool -> Bool -> Bool
oBien2 True _ = True
oBien2 _ b = b



--4. REGISTROS

--Ejercicio 4.1

--a.

data Persona = P String Int
               -- nombre, edad 
 deriving Show


nicolePersona = P "Nicole" 25
martaPersona = P "Marta" 26

{-
nombre :: Persona -> String    
nombre (P n e) = n  
-}

 -- funcion observadora
nombre :: Persona -> String
nombre p = nom p

--matching
nom :: Persona -> String
nom (P n e) = n

--b

edad :: Persona -> Int
edad (P n e) = e

--c

crecer :: Persona -> Persona
crecer (P n e) = P n (e + 1)

--d

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre s (P n e) = P s e

--e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

--f

laQueEsMayor :: Persona -> Persona -> Persona
-- si las dos personas tienen la misma edad, retornará la segunda persona pasada por argumento
laQueEsMayor p1 p2 = if edad p1 > edad p2
                        then p1
                        else p2

-- Ejercicio 4.2

data Pokemon = POKE  TipoDePokemon    Int      
     deriving Show             
                    --TipoDePokemon  Porcentaje de energía  

data Entrenador = E    String Pokemon Pokemon
     deriving Show
                    -- Nombre Pokemon Pokemon

data TipoDePokemon = Agua | Fuego | Planta deriving (Eq, Show)
     

--a

superaA :: Pokemon -> Pokemon -> Bool
superaA (POKE t1 _) (POKE t2 _) = tipoDelPokemonEsSuperior t1 t2

tipoDelPokemonEsSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
tipoDelPokemonEsSuperior Agua Fuego = True
tipoDelPokemonEsSuperior Fuego Planta = True
tipoDelPokemonEsSuperior Planta Agua = True
tipoDelPokemonEsSuperior _ _ = False

-- Creación de dos pokes para chequear

pokemon1 = POKE Planta 20
pokemon2 = POKE Fuego 10

entrenador1 = E "Nicole" pokemon1 pokemon2
entrenador2 = E "Nacho" pokemon1 pokemon2
--b
{-}
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tp (E _ p1 p2) = 
                                if (tipoDelPokemon p1 == tp) && (tipoDelPokemon p2 == tp)
                                    then 2
                                    else if (tipoDelPokemon p1 == tp) || (tipoDelPokemon p2 == tp)
                                        then 1
                                        else 0
  otra manera de hacerlo usando deriving Eq                                      
-}
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tp (E _ p1 p2) = unoMismoTipoDelPokemonCeroSino (tipoDelPokemon p1) tp + unoMismoTipoDelPokemonCeroSino (tipoDelPokemon p2) tp
                                               
                                                                    

tipoDelPokemon :: Pokemon -> TipoDePokemon
tipoDelPokemon (POKE t _ )= t          


unoMismoTipoDelPokemonCeroSino :: TipoDePokemon -> TipoDePokemon -> Int
unoMismoTipoDelPokemonCeroSino Agua Agua = 1
unoMismoTipoDelPokemonCeroSino Planta Planta = 1
unoMismoTipoDelPokemonCeroSino Fuego Fuego = 1
unoMismoTipoDelPokemonCeroSino _ _ = 0

mismoTipoDelPokemon :: TipoDePokemon -> TipoDePokemon -> Bool
mismoTipoDelPokemon Agua Agua = True
mismoTipoDelPokemon Planta Planta = True
mismoTipoDelPokemon Fuego Fuego = True
mismoTipoDelPokemon _ _ = False

--c

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (E _ p1 p2, E _ p3 p4) = p1:p2:p3:p4:[]


-- 5. FUNCIONES POLIMÓRFICAS

-- Ejercicio 5.1

--a.
loMismo :: a -> a
loMismo x = x

--b
siempreSiete :: a -> Int
siempreSiete x = 7


--c
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

tupla1 = (7,7)

{-
    ¾Por qué existen dos variables de tipo diferentes?
    Porque las tuplas pueden ser de diferentes tipos


-}

--Ejercicio 5.2

{-
    Responda la siguiente pregunta: ¾Por qué estas funciones son polimórficas?
    Porque no tienen un tipo predefinido de datos que reciben y devuelven, permiten recibir cualquier tipo de dato haciendo que sean de uso genérico.


-}

-- 6. PATTERN MATCHING SOBRE LISTAS

-- 2

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

--3
lista1 = [1, 2 ,3]

elPrimero :: [a] -> a
elPrimero (a : _) = a


--4

sinElPrimero :: [a] -> [a]
sinElPrimero (a : b) = b

--5

splitHead :: [a] -> (a, [a])
splitHead [] = error "La lista está vacía"
splitHead (x:xs) = (x, xs)

